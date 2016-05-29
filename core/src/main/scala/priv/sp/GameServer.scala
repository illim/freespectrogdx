package priv.sp

import priv.sp.bot._
import priv.util.Utils._
import priv.util.TVar
import scala.util.Random

/**
 * Player plays against game server hiding that it can be local(ai) or remote.
 * In remote play, the master shuffle the cards and send to the slave.
 *
 */
trait GameServer {
  def initState(): GameState
  def desc(): GameDesc
  def playerId: PlayerId
  def startingPlayer: PlayerId
  def name: String
  def seed: Long
  resetSeed()

  // None implies surrender
  def waitNextCommand(c: TVar[Option[Option[Command]]], state: GameState)
  // send the user command to opponent
  def submitCommand(commandOption: Option[Command])
  def resetSeed() { Random.setSeed(seed) }
  def reset() {}
  def surrender() {}
  var abort = { () ⇒ }
}

class Local(resources: GameResources) extends GameServer {
  Random.setSeed(System.currentTimeMillis)
  private val shuffle = new CardShuffle(resources.sp.houses)
  val startingPlayer = playerIds(scala.util.Random.nextInt(2))
  println("starting player:" + startingPlayer)
  private val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get(resources.resolveChoices, startingPlayer)

  def initState = GameState(List(PlayerState.init(p1State, p1Desc), PlayerState.init(p2State, p2Desc)))
  val desc = GameDesc(Vector(p1Desc, p2Desc))
  val playerId = opponent
  val bot = new BoundedBot2(playerId, desc, resources.sp.houses)
  val name = "AI"
  val seed = System.currentTimeMillis
  def waitNextCommand(c: TVar[Option[Option[Command]]], state: GameState) = {
    resources.aiExecutor submit runnable(c.set(Some(bot.executeAI(state))))
  }

  def submitCommand(commandOption: Option[Command]) = {
    commandOption.foreach { c ⇒
      val cardIdx = desc.players(owner) getIndexOfCardInHouse c.card
      if (cardIdx != -1) {
        resources.aiExecutor submit runnable(bot.knowledge.updateKnowledge(c, cardIdx))
      }
    }
  }

  override def reset() {
    super.reset()
    resources.aiExecutor submit runnable(bot.knowledge.reset())
  }
}
