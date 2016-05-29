package priv.sp.update

import priv.sp.GameCardEffect.oneTimePlayerEffect

import collection._
import priv.sp._
import priv.util.FieldUpdate
import CardSpec._

import scala.reflect.ClassTag

class PlayerUpdate(val id: PlayerId, val updater: GameStateUpdater) extends FieldUpdate(Some(updater), updater.state.players(id)) { playerFieldUpdate ⇒
  def pstate = value
  def updateListener = updater.updateListener
  def ended = updater.ended
  val slotsUpdate = new SlotsUpdate(this)
  val housesUpdate = new HousesUpdate
  val houseEventListener = updater.desc.players(id).houses(4).house.eventListener.map {
    case OpponentListener(f) ⇒
      updater.desc.players(other(id)).houses(4).house.eventListener.collect {
        case c: CustomListener ⇒ f(c())
      } getOrElse f(new HouseEventListener)
    case c: CustomListener ⇒ c()
  } getOrElse new HouseEventListener

  val stats = PlayerStats()
  val otherId = other(id)
  protected lazy val otherPlayerStats = updater.playerFieldUpdates(otherId).stats
  def otherHouseEventListener = updater.houseEventListeners(otherId)

  def otherPlayer = updater.players(other(id)) // not great
  def getHouses = if (housesUpdate.isDirty) housesUpdate.value else value.houses
  def getSlots = if (slotsUpdate.isDirty) slotsUpdate() else value.slots // horrible perf probably
  def slots = slotsUpdate.reinit()
  def houses = housesUpdate.reinit()
  // hack? for example avoid case of card moving during mass damage
  // not great on dead effect happens maybe too late
  def flush() = {
    if (isDirty) {
      if (slotsUpdate.isDirty) {
        slotsUpdate.logs.reverseIterator foreach {
          case dead: Dead ⇒
            stats.nbDead += 1
            otherPlayerStats addKill dead.slot
            slots onDead dead
          case _ ⇒
        }
        slots.logs = Nil
      }
    }
  }
  def result = value.copy(slots = getSlots, houses = getHouses)

  def runSlots(): Unit = {
    getSlots.foreach {
      case (num, s) ⇒
        val slots = getSlots
        (slots get num) foreach { slot ⇒
          if (slot.id != s.id) { // in case of a move
            slots.find(_._2.id == s.id) foreach { case (num, slot) ⇒ runSlot(num, slot) }
          } else runSlot(num, slot)
        }
    }
  }

  def runSlot(numSlot: Int, slot: SlotState): Unit = {
    if (slot.attack > 0 && slot.isRunnable && ended.isEmpty) {
      val d = Damage(slot.attack, Context(id, Some(slot.card), numSlot))
      updateListener.runSlot(numSlot, id)
      slot.card.runAttack(slot.target, d, this)
      updateListener.refresh()
    }
  }

  def runSlot(s: SlotUpdate): Unit = {
    s.value foreach { slot ⇒
      runSlot(s.num, slot)
    }
  }

  /**
   * Note : direct effects are applieds after add and their callbacks which is different
   * from Spectromancer. Example : trooper damages a summoned creature
   * before being killed by his effect. (alternative is to create a phase beforeadd, afteradd)
   */
  def submit(c: Option[Command]) = {
    val (test, newCommand) = houseEventListener interceptSubmit c
    (if (test) newCommand else {
      val (test2, newCommand2) = otherHouseEventListener interceptSubmit c
      if (test2) newCommand2 else c
    }) foreach submitCommand
  }

  var submitCommand : Function[Command, Option[Int]]= { command: Command ⇒
    if (command.card.isSpell) {
      updateListener spellPlayed command
    }
    houses.incrMana(-command.cost, command.card.houseIndex) // could be after but it's ugly(-> hack for fury)
    updateListener.refresh(silent = true)
    val slotIdOption = if (!command.card.isSpell) {
      command.input map { slotInput ⇒
        val targetSlots = command.card.inputSpec match {
          case Some(SelectTargetCreature) ⇒
            otherPlayer.slots
          case _ ⇒
            slots
        }
        stats.nbSummon += 1
        targetSlots.summon(slotInput.num, command.card.asCreature)
      }
    } else None
    command.card.effects(CardSpec.Direct) foreach { f ⇒
      val env = new GameCardEffect.Env(command.player, updater)
      env.card = Some(command.card)
      command.input foreach { slotInput ⇒
        env.selected = slotInput.num
      }
      f(env)
    }
    updateListener.refresh()
    slotIdOption
  }

  def inflict(d: Damage) = {
    if (ended.isEmpty) {
      val amount = guard(mod(d)).amount
      val life = value.life - amount
      if (life <= 0 && houseEventListener.onDeath()) {
        updater.ended = Some(other(id))
      } else {
        onPlayerDamage(d.copy(amount = amount))
      }

      write(value.copy(life = life))
    }
  }

  // bs for mk, antimancer
  var onPlayerDamage = { d : Damage =>

  }


  def heal(amount: Int) = {
    if (ended.isEmpty) {
      if (value.life > 0){
        write(value.copy(life = value.life + amount))
      }
    }
  }

  def addEffect(effect: CardSpec.PhaseEffect) = {
    write(value.copy(effects = effect :: value.effects))
  }

  def addEffectOnce(effect: CardSpec.PhaseEffect) = {
    addEffect(effect._1 -> oneTimePlayerEffect(effect._2))
  }

  def removeEffect(cond: CardSpec.Effect ⇒ Boolean) = {
    write(value.copy(effects = value.effects filter (e ⇒ !cond(e._2))))
  }

  def mapEffect(f: CardSpec.Effect ⇒ CardSpec.Effect) = {
    write(value.copy(effects = value.effects map (x ⇒ (x._1, f(x._2)))))
  }

  var setData = { (data: AnyRef) =>  write(value.copy(data = data)) }
  def updateData[A <: AnyRef](f: A ⇒ A)(implicit c : ClassTag[A]) = {
    if (c.runtimeClass.isInstance(value.data)) {
      setData(f(value.data.asInstanceOf[A]))
    }
  }

  def addTransition(t: Transition) = {
    write(value.copy(transitions = t :: value.transitions))
  }

  def popTransition: Option[Transition] = {
    value.transitions match {
      case Nil ⇒ None
      case head :: tail ⇒
        write(value.copy(transitions = tail))
        Some(head)
    }
  }

  def blockSlot(n: Int) = {
    write(value.copy(slotList = value.slotList.filterNot(_ == n)))
  }

  private val playerEffectEnv = new GameCardEffect.Env(id, updater)
  private val slotEffectEnv = new GameCardEffect.Env(id, updater)

  // why player effects happen after? (I don't remember)
  def applyEffects(phase: CardSpec.Phase) = {
    getSlots foreach {
      case (num, s) ⇒
        if (ended.isEmpty) {
          val slots = getSlots
          (slots get num) foreach { slot ⇒ // looks weird because slots can change at each iteration
            if (slot.id != s.id) { // in case of moved creature
              (slots find (_._2.id == s.id)) foreach { case (num, slot) ⇒ applyEffect(phase, slot, num) }
            } else applyEffect(phase, slot, num)
          }
        }
    }
    value.effects foreach {
      case (p, f) ⇒
        if (p == phase && ended.isEmpty) {
          f(playerEffectEnv)
        }
    }
  }

  @inline def applyEffect(phase: CardSpec.Phase, slot: SlotState, num: Int) = {
    if (!slot.has(CardSpec.blockedFlag)) {
      slot.card.effects(phase) map { f ⇒
        slotEffectEnv.selected = num
        slotEffectEnv.card = Some(slot.card)
        f(slotEffectEnv)
      }
      updateListener.refresh(silent = true)
    }
  }

  def prepareNextTurn() = {
    houses.incrMana()
  }

  def mod(d: Damage): Damage = {
    if (d.context.playerId == id) otherPlayer mod d
    else {
      if (d.isSpell) {
        (updater.houseEventListeners(d.context.playerId).mod(d) /: otherPlayer.getSlots) {
          case (acc, (_, slot)) ⇒
            slot.card.mod match {
              case Some(SpellMod(mod)) ⇒ acc.copy(amount = mod(acc.amount))
              case _                   ⇒ acc
            }
        }
      } else d
    }
  }

  def addDescMod(dmods: DescMod*) = { write(value.copy(desc = value.desc.add(dmods: _*))) }
  def insertDescMod(dmod: DescMod) = { write(value.copy(desc = value.desc.insert(dmod))) }
  def removeDescMod(dmod: DescMod) = { write(value.copy(desc = value.desc.remove(dmod))) }

  // sub optimal?
  var guard = { damage: Damage =>
    slots.foldl(damage) {
      case (acc, slot) ⇒
        slot.get.reaction onProtect DamageEvent(acc, None, this)
    }
  }

  class HousesUpdate extends FieldUpdate(Some(playerFieldUpdate), pstate.houses) {
    def houses = value

    def incrMana(incr: Int = 1) = {
      write(houses map { house ⇒
        addMana((house, incr))
      })
      updateElementals()
    }

    def incrMana(amount: Int, houseIndexes: Int*) = {
      write(
        (houseIndexes foldLeft houses) { (acc, id) ⇒
          val house = acc(id)
          acc.updated(id, addMana((house, amount)))
        })
      updateElementals()
    }

    var addMana : Function[(HouseState, Int), HouseState] = { case ((houseState, amount)) =>
      new HouseState(math.max(0, houseState.mana + amount))
    }

    def updateElementals() = {
      if (getSlots.exists(_._2.card.attack.base.isEmpty)) { // bs
        slots foreach { slot ⇒
          if (slot.attack.get.base.isEmpty) {
            slot.attack.setDirty() // force reeval
          }
        }
      }
    }
  }
}

