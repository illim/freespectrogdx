package priv.sp.update

import collection._
import priv.sp._
import priv.util.FieldUpdate

// crap most functions suppose value is tested if well defined
class SlotUpdate(val num: Int, val slots: SlotsUpdate) extends FieldUpdate(Some(slots), slots.value.get(num)) {
  val updater = slots.updater
  val player = slots.player
  val playerId = player.id
  val otherPlayerId = other(playerId)
  val attackUpdate = new AttackUpdate(this)
  lazy val adjacentSlots: List[SlotUpdate] = adjacents(num).map { n ⇒ slots(n) }
  def otherPlayer = updater.players(otherPlayerId)

  def oppositeSlot = otherPlayer.slots(num)
  def filledAdjacents = adjacentSlots filter (_.value.isDefined)
  def openAdjacents   = adjacentSlots filter (s => s.value.isEmpty && player.value.isInSlotRange(s.num))
  def oppositeState: Option[SlotState] = otherPlayer.getSlots get num

  def get = {
    value match {
      case Some(x) => x
      case None => throw new Exception("empty slot " + num)
    }
  }
  // some crap
  def toggleRun() {
    value.foreach { x ⇒
      if (!x.has(CardSpec.runFlag) || x.has(CardSpec.stunFlag)) {
        write(value.map(x ⇒ x.copy(status = (x.status | CardSpec.runFlag) & (~CardSpec.stunFlag))))
      }
    }
  }
  def toggle(flag: Int)     = { write(value map (x ⇒ x.copy(status = x.status | flag))) }
  def toggleOff(flag: Int)  = { write(value map (x ⇒ x.copy(status = x.status & (~flag)))) }
  def setData(data: AnyRef) = { write(value map (_.copy(data = data))) }
  def setTarget(target: List[Int]) = { write(value map (_.copy(target = target))) }
  def focus(blocking: Boolean = true) = { slots.updateListener.focus(num, playerId, blocking) }

  // -- code horror to intercept heal/inflict
  def heal(amount: Int) = { if (value.isDefined && get.life > 0) get.reaction heal amount }
  def inflict(damage: Damage) = {
    if (value.isDefined) get.reaction inflict (player mod damage)
  }
  def destroy() = { if (value.isDefined) { get.reaction.destroy() } }
  def stun() { if (value.isDefined) { get.reaction.stun() } }
  var overridableDestroy : () => Unit = { () => destroy() }
  // --

  def attack = attackUpdate.reinit()
  def apply() = updated {
    value.map { s ⇒
      if (attackUpdate.isDirty) {
        val attackSources = attackUpdate.value.get
        s.copy(attack = slots.getAttack(this, attackSources), attackSources = attackSources)
      } else s
    }
  }

  def add(card: Creature) { add(slots.buildSlotState(this, card)) }
  var add : Function[SlotState, Unit] = { slot: SlotState ⇒
    write(Some(slot))
    slot.reaction use this
    slots reactAdd this
  }

  // /!\ don't call it directly (use inflict)
  def damageSlot(damage: Damage) = {
    if (value.isDefined) {
      val slotState = get
      val d = protect(slotState.reaction selfProtect damage) // /!\ possible side effect (jf can protect herself once and toggle a flag)
      val slot = get
      val amount = slot inflict d match {
        case None ⇒
          delayedDestroy(d)
          slot.life
        case Some(newslot) ⇒
          write(Some(newslot))
          slot.life - newslot.life
      }
      onDamage((slot, damage.copy(amount = amount)))
    }
  }

  var onDamage : Function[(SlotState, Damage), Unit] = { case ((slotState : SlotState, d : Damage)) =>
    slotState.reaction onMyDamage d
  }

  def drain(damage: Damage) {
    if (value.isDefined) {
      val slot = get
      val newslot = slot.copy(life = slot.life - damage.amount)
      val amount = if (newslot.life < 1) {
        delayedDestroy(damage)
        slot.life
      } else {
        write(Some(newslot))
        slot.life - newslot.life
      }
      onDamage((slot, damage.copy(amount = amount)))
    }
  }

  val protect = new priv.util.FuncDecorator1({ d: Damage ⇒
    d
  })

  var remove : Function[Option[Dead], SlotState]= { deadOpt: Option[Dead] ⇒
    val slotState = get
    slotState.reaction onMyRemove deadOpt
    slots reactRemove this
    val result = apply().getOrElse(slotState) // ??? in case MK attack on die and die again against zen guard?
    slotState.reaction.cleanUp()
    attackUpdate.invalidate() // FIXME hack?
    write(None)
    result
  }

  var delayedDestroy : Function[Damage, Unit] = { d: Damage =>
    val s = get
    val event = Dead(num, s, player, Some(d))
    remove(Some(event))
    slots log event
  }

  def privDestroy() = {
    val s = get
    val event = Dead (num, s, player, None)
    remove (Some (event) )
    slots onDead event
  }
}

class AttackUpdate(slot: SlotUpdate) extends FieldUpdate(Some(slot), slot.value map (_.attackSources)) {
  @inline def get = value getOrElse sys.error("can't get attack of empty slot " + slot.num)
  def add(source: AttackSource) { if (get.base != some0) write(Some(get add source)) }
  def removeFirst(source: AttackSource) { write(value map (_ removeFirst source)) }
  def removeFirstEq(source: AttackSource) { write(value map (_ removeFirstEq source)) }
  def removeAny(source: AttackSource) { write(value map (_ removeAny source)) }
  def has[A: reflect.ClassTag] = get.sources.exists {
    case _: A ⇒ true
    case _    ⇒ false
  }
}
