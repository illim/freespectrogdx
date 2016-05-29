package priv.sp.update

import priv.sp._
import priv.util.FieldUpdate

class SlotsUpdate(val player: PlayerUpdate) extends FieldUpdate(Some(player), player.value.slots) {
  import player._
  var logs = List.empty[BoardEvent]
  private val slotUpdates = baseSlotRange.map { i ⇒ new SlotUpdate(i, this) }.toVector
  private val filledSlots = slotUpdates.withFilter(_.value.isDefined)
  private val first = slotUpdates(0)
  val playerId = player.id

  def updater = player.updater
  def updateListener = updater.updateListener

  def apply() = updated {
    var res = PlayerState.emptySlots
    slotUpdates foreach { slot ⇒
      slot() foreach { s ⇒
        res += (slot.num -> s)
      }
    }
    res
  }

  def slots = { ensureInited(); slotUpdates }
  def filledsM = { ensureInited(); filledSlots }
  def filleds = slots.filter(_.value.isDefined)
  def getOpenSlots = { ensureInited(); slotUpdates filter (s ⇒ s.value.isEmpty && player.value.isInSlotRange(s.num)) }
  def apply(n: Int) = slots(n)
  def ensureInited() = if (!first.isInited) {
    slotUpdates.foreach { s ⇒
      s.reinit()
      s.value foreach (_.reaction.use(s))
    }
  }

  def inflictCreatures(damage: Damage) {
    val d = mod(damage)
    foreach(_.get.reaction.inflict(d))
  }

  def summon(num: Int, card: Creature) : Int = {
    val slot = slots(num)
    var slotState = buildSlotState(slot, card)
    slot.value foreach { s ⇒
      s.reaction onOverwrite card
      slotState.reaction.onSpawnOver foreach { m ⇒
        slotState = m(slotState)
      }
    }
    val summoned = slot.value.isEmpty
    if (summoned) {
      slot add slotState
    }
    updateListener.summon(num, slotState, id)
    if (summoned) {
      val summonEvent = SummonEvent(num, card, player)
      otherPlayer.slots reactSummon summonEvent
      reactSummon(summonEvent)
    }
    slotState.id
  }

  def move(num: Int, dest: Int, destPlayerId : PlayerId = playerId) {
    if (player.value isInSlotRange dest) {
      val slot = slots(num)
      slot.value foreach { s ⇒
        val slotDest = updater.players(destPlayerId).slots(dest)
        if (slotDest.value.isDefined) {
          swap(slot, slotDest, destPlayerId)
        } else {
          updateListener.move(num, dest, id, destPlayerId)
          val removed = slot remove None
          privMove(removed, slotDest)
        }
      }
    }
  }

  private def swap(slot : SlotUpdate, dest : SlotUpdate, destPlayerId : PlayerId): Unit = {
    updateListener.swap(slot.num, dest.num, id, destPlayerId)
    val removed = slot remove None
    val removed2 = dest remove None
    privMove(removed, dest)
    privMove(removed2, slot)
  }

  private def privMove(slotState : SlotState, to : SlotUpdate) : Unit = {
    to add SlotState(
      slotState.card, slotState.life, slotState.maxLife, slotState.status,
      slotState.attackSources, getAttack(to, slotState.attackSources),
      List(to.num), slotState.id, slotState.reaction, slotState.data)
    slotState.reaction use to
  }

  // If observed , because of simple onMyDeath(ie selfProtect, onMyDamage) method to be preferred, selected slot may be filtered out
  var onDead : Function[Dead, Unit] = { dead: Dead ⇒
    updateListener.die(dead.num, playerId)
    dead.slot.reaction onMyDeath dead
  }

  def buildSlotState(slot: SlotUpdate, card: Creature, id: Int = SlotState.currentId.incrementAndGet) = {
    val reaction = card.newReaction
    reaction use slot
    SlotState(card, card.life, card.life, card.status, card.attack, getAttack(slot, card.attack), List(slot.num), id, reaction, card.data)
  }
  def getAttack(slot: SlotUpdate, attackSources: AttackSources) = {
    (attackSources.base.getOrElse(0) /: attackSources.sources) { (acc, s) ⇒
      s match {
        case f: AttackFunc          ⇒ f(acc)
        case f: AttackStateFunc     ⇒ f(acc, player)
        case f: AttackSlotStateFunc ⇒ f(acc, slot)
        case _                      ⇒ acc
      }
    }
  }
  def toggleRun() = foreach(_.toggleRun())
  def healCreatures(amount: Int) = foreach(_.heal(amount))
  def reactSummon(e: SummonEvent) = foreach { s ⇒
    if (playerId != e.player.id || s.num != e.num) {
      s.get.reaction.onSummon(e)
    }
  }
  def reactAdd(slot: SlotUpdate) = foreach { s ⇒
    s.get.reaction onAdd slot
  }
  def reactRemove(slot: SlotUpdate) = foreach { s ⇒
    s.get.reaction onRemove slot
  }
  def log(evt: BoardEvent) { logs = evt :: logs }
  def foreach(f: SlotUpdate ⇒ Unit) = slots.foreach { s ⇒
    if (s.value.isDefined) {
      f(s)
    }
  }
  def foldl[B](z: B)(f: (B, SlotUpdate) ⇒ B) = {
    var r = z
    foreach { s ⇒ r = f(r, s) }
    r
  }
  def reduce(f: (SlotUpdate, SlotUpdate) ⇒ SlotUpdate): Option[SlotUpdate] = {
    foldl(Option.empty[SlotUpdate]) {
      case (None, s)      ⇒ Some(s)
      case (Some(s0), s1) ⇒ Some(f(s0, s1))
    }
  }
  def findCard(card: Card): Option[SlotUpdate] = {
    slots.find(s ⇒ s.value.isDefined && s.get.card == card)
  }
  def findSlot(id: Int): Option[SlotUpdate] = {
    slots.find(s ⇒ s.value.isDefined && s.get.id == id)
  }
}
