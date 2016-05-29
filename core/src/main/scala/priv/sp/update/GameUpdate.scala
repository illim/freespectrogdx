package priv.sp.update

import scala.util.Random
import collection._
import annotation._
import priv.sp._
import priv.util.FieldUpdate

// desc is only used to get house specific listener
class GameStateUpdater(initState: GameState, val desc: GameDesc) extends FieldUpdate(None, initState) { self ⇒
  val playerFieldUpdates = playerIds.map(id ⇒ new PlayerUpdate(id, self))
  var ended = Option.empty[PlayerId]
  var updateListener: UpdateListener = new DefaultUpdateListener
  val houseEventListeners = playerFieldUpdates map (_.houseEventListener)
  val stats = playerFieldUpdates map (_.stats)
  playerFieldUpdates foreach { p ⇒
    p.houseEventListener init p
  }
  var randLogs = new RandLogs

  def state = value

  def apply(value: GameState) = {
    ended = None
    initNewUpdate(value)
  }

  def lift[R](f: GameStateUpdater ⇒ R): GameState => (GameState, R) = { st: GameState ⇒
    val update = apply(st)
    val res = f(update)
    (update.result, res)
  }

  def resetRand() = {
    val r = randLogs
    randLogs = new RandLogs
    r
  }

  def focus(num: Int, playerId: PlayerId, blocking: Boolean = true) = updateListener.focus(num, playerId, blocking)

  def players(id: PlayerId) = playerFieldUpdates(id).reinit()

  def resetStats() = { playerFieldUpdates foreach (_.stats.reset()) }
  def flush() = { playerFieldUpdates foreach (_.flush()) }

  def result = {
    flush()
    state.copy(players = playerIds.map { id ⇒
      val fp = playerFieldUpdates(id)
      if (fp.isDirty) fp.result else state.players(id)
    })
  }
}

trait UpdateListener {
  def focus(num: Int, playerId: PlayerId, blocking: Boolean = true): Unit
  def move(num: Int, dest: Int, playerId: PlayerId, destPlayerId : PlayerId): Unit
  def swap(num: Int, dest: Int, playerId: PlayerId, destPlayerId : PlayerId): Unit
  def runSlot(num: Int, playerId: PlayerId): Unit
  def summon(num: Int, slot: SlotState, playerId: PlayerId): Unit
  def die(num: Int, playerId: PlayerId): Unit
  def refresh(silent: Boolean = false): Unit // this is not great to have some gui code here
  def spellPlayed(c: Command): Unit
  def triggerAbility(o: AnyRef): Unit
}

class DefaultUpdateListener extends UpdateListener {
  def focus(num: Int, playerId: PlayerId, blocking: Boolean): Unit = {}
  def move(num: Int, dest: Int, playerId: PlayerId, destPlayerId : PlayerId): Unit = {}
  def swap(num: Int, dest: Int, playerId: PlayerId, destPlayerId : PlayerId): Unit = {}
  def runSlot(num: Int, playerId: PlayerId): Unit = {}
  def summon(num: Int, slot: SlotState, playerId: PlayerId): Unit = {}
  def die(num: Int, playerId: PlayerId): Unit = {}
  def refresh(silent: Boolean): Unit = {}
  def spellPlayed(c: Command): Unit = {}
  def triggerAbility(o: AnyRef): Unit = {}
}

// broadcast crap
class HouseEventListener {
  protected var playerField: PlayerUpdate = null
  def player = playerField.reinit()

  def mod(damage: Damage) = damage
  def onDeath() : Boolean = true // bs for limbo
  def interceptSubmit(c: Option[Command]): (Boolean, Option[Command]) = Reaction.falseNone
  def init(p: PlayerUpdate) = { playerField = p }
}

// bs for warp class
class ProxyEventListener(inner: HouseEventListener) extends HouseEventListener {

  override def onDeath() = { inner.onDeath() }
  override def interceptSubmit(c: Option[Command]): (Boolean, Option[Command]) = inner.interceptSubmit(c)
  override def init(p: PlayerUpdate): Unit = {
    playerField = p
    inner init p
  }
}

// horrible tracking of rand usage
case class RandLog(var count: Int)
class RandLogs {
  var offset = 0
  var logs = Vector.empty[RandLog]

  def get(n: Int) = {
    val randlog = getOrAddLog(n)
    Random.nextInt(n)
  }

  def unorderedShuffle[A](l: Seq[A], count: Int = -1) = {
    val limit = if (count == -1) 1 else l.size - count + 1
    val randlog = getOrAddLog(fact(l.size, limit) / 2)
    if (limit == 1) Random.shuffle(l) else Random.shuffle(l).take(count) // crap
  }

  def getOrAddLog(n: Int) = {
    if (logs.size == offset) {
      val r = RandLog(n)
      logs = r +: logs
      offset += 1
      r
    } else {
      val r = logs(logs.size - offset)
      if (r.count < n) r.count = n
      offset += 1
      r
    }
  }

  def width = (1 /: logs) { (acc, l) ⇒
    acc * l.count
  }

  def probability: Float = 1f / width

  @tailrec private def fact(n: Int, limit: Int, acc: Int = 1): Int = if (n == limit) acc else fact(n - 1, limit, acc * n)
}
