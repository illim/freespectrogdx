package priv.sp

import collection._
import java.io._
import priv.sp.update._

object CardSpec {
  val runFlag = 1
  val stunFlag = 2
  val invincibleFlag = 4
  val blockedFlag = 8
  val pausedFlag = 16
  val cursedFlag = 32

  val onHold = stunFlag + blockedFlag + pausedFlag

  type Phase = Int
  val Direct = 0
  val OnTurn = 1
  val OnEndTurn = 2
  val OnStart = 3
  val phases = Array(Direct, OnTurn, OnEndTurn, OnStart)

  type Effect = GameCardEffect.Env ⇒ Unit
  type PhaseEffect = (CardSpec.Phase, CardSpec.Effect)
  type Description = (GameState, PlayerId) => String
  implicit def toDescription(s : String) : Description = (_, _) => s

  def effects(effects: PhaseEffect*) = toEffectMap(effects)

  def toEffectMap(effects: Traversable[PhaseEffect]) = {
    def effectAt(phase: Phase): Option[Effect] = {
      val filtereds = effects collect { case (ph, f) if ph == phase ⇒ f }
      if (filtereds.isEmpty) None
      else if (filtereds.size == 1) Some(filtereds.head)
      else Some(new ComposedEffect(filtereds))
    }

    phases map (effectAt _)
  }
  val noEffects = phases map (_ ⇒ Option.empty[Effect])

  class ComposedEffect(effects: Traversable[Effect]) extends Function[GameCardEffect.Env, Unit] {
    def apply(env: GameCardEffect.Env) = {
      effects foreach (_(env))
    }
  }
  val defaultReaction = () ⇒ new Reaction
}

import CardSpec._

object Card {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}

sealed abstract class Card  {
  def name: String
  def image: String
  def inputSpec: Option[CardInputSpec]
  def effects: Array[Option[Effect]]
  def description: Description

  var cost = 0
  var id = Card.currentId.incrementAndGet
  var houseId = 0
  var houseIndex = 0
  var cardIndex = 0
  def cardIndex1 = cardIndex + 1
  final val isSpell = isInstanceOf[Spell]
  def isSpecial = houseIndex == 4
  def asCreature = {
    this match {
      case creature: Creature ⇒ creature
      case _                  ⇒ sys.error(this + " is not a creature")
    }
  }
  override def toString() = s"Card($name)"
  override def hashCode(): Int = id
  override def equals(o: Any) = {
    o match {
      case c: Card ⇒ c.hashCode() == hashCode()
      case _       ⇒ false
    }
  }
}

class Creature(
    val name: String,
    val attack: AttackSources,
    val life: Int,
    val description: Description = "",
    val inputSpec: Option[CardInputSpec] = Some(SelectOwnerSlot),
    var effects: Array[Option[CardSpec.Effect]] = CardSpec.noEffects,
    val mod: Option[Mod] = None,
    reaction: ⇒ Reaction = new Reaction,
    val data: AnyRef = null, // initialize slot custom data
    val runAttack: RunAttack = SingleTargetAttack,
    val isAltar: Boolean = false,
    val status: Int = 0) extends Card {

  def this() = this(null, AttackSources(), 0)

  def newReaction = reaction

  final def inflict(damage: Damage, life: Int) = life - damage.amount

  def image = name + ".jpg"
}

case class Spell(
    name: String,
    description: Description = "",
    inputSpec: Option[CardInputSpec] = None,
    effects: Array[Option[CardSpec.Effect]] = CardSpec.noEffects) extends Card {
  def this() = this(null)
  def image = name + ".tga"
}
trait CommandFlag
case class Command(player: PlayerId, card: Card, input: Option[SlotInput], cost: Int, flag: Option[CommandFlag] = None) {
  final override def toString() = player + "> " + card.name + input.map(i ⇒ " at " + i.num).getOrElse("")
}

object Context {
  val noSelection = -1
  def apply(playerId: PlayerId, card: Option[Card] = None, selected: Int = Context.noSelection) = new ContextImpl(playerId, card, selected)
}
trait Context {
  def playerId: PlayerId
  def card: Option[Card]
  def selected: Int
  def selectedOption = if (selected == Context.noSelection) None else Some(selected)
}
// context is mostly for interception, so it's not that important if it's not supplied for self damage for example
class ContextImpl(val playerId: PlayerId, val card: Option[Card], val selected: Int) extends Context

case class Damage(amount: Int, context: Context, isAbility: Boolean = false, isSpell: Boolean = false) {
  def isEffect = isAbility || isSpell
}

sealed trait CardInputSpec
trait OwnerInputSlot
case class SelectOwner(f: (PlayerId, GameState) ⇒ Seq[Int]) extends CardInputSpec with OwnerInputSlot
case object SelectOwnerSlot extends CardInputSpec with OwnerInputSlot
case object SelectOwnerCreature extends CardInputSpec with OwnerInputSlot
case class SelectTarget(f: (PlayerId, GameState) ⇒ Seq[Int]) extends CardInputSpec
case object SelectTargetSlot extends CardInputSpec
case object SelectTargetCreature extends CardInputSpec

class SlotInput(val num: Int) extends AnyVal with Serializable


trait Mod
case class SpellMod(modify: Int ⇒ Int) extends Mod

sealed trait BoardEvent
trait PlayerEvent extends BoardEvent {
  def player: PlayerUpdate
  def otherPlayer = player.otherPlayer
}
case class Dead(num: Int, slot: SlotState, player: PlayerUpdate, damage: Option[Damage]) extends PlayerEvent {
  def isEffect = damage.isEmpty || damage.get.isEffect
  def isSpell = damage.isEmpty || damage.get.isSpell
  def isDestroy = damage.isEmpty
  def card = slot.card
}
// need source if no target
case class DamageEvent(damage: Damage, target: Option[Int], player: PlayerUpdate) extends PlayerEvent
case class SummonEvent(num: Int, card: Creature, player: PlayerUpdate) extends PlayerEvent

trait SlotMod {
  def apply(slotState: SlotState): SlotState
}

// some crap
trait Actions {
  protected var selected: SlotUpdate = null
  def use(s: SlotUpdate) { selected = s }
  def heal(amount: Int) { selected write Some(SlotState.addLife(selected.get, amount)) }
  def inflict(damage: Damage) { selected damageSlot damage }
  def destroy() { selected.privDestroy() }
  def stun() { selected toggle CardSpec.stunFlag }
}
object Reaction {
  val falseNone = (false, None)
}
class Reaction extends Actions {
  def onAdd(slot: SlotUpdate) {}
  def onRemove(slot: SlotUpdate) {}

  // used by stone golem and archphoenix where overriding inflict doesn't suffice, because needs to know the context => TODO remove overrided inflict
  def selfProtect(d: Damage) = d
  // used by black monk to heal by the amount even when dying, and by errant to wakeup
  def onMyDamage(damage : Damage) {}
  // /!\ the slot is not yet empty but is about to (used for f5, f7, schizo, crossbow)
  def onMyRemove(dead: Option[Dead]) {}
  def onMyDeath(dead: Dead) {}
  // TODO call this from house listener?
  def onSummon(summoned: SummonEvent) {}
  def onSpawnOver: Option[SlotMod] = { selected.destroy(); None }
  def onOverwrite(c: Creature) {}
  def cleanUp() {} // bs for warp

  /**
   * Events that needs to be broadcasted manually in a house listener
   */
  // broadcast is already done for player target (for ice guard)
  def onProtect(d: DamageEvent) = d.damage
  def onDeath(dead: Dead) {}
}

trait RunAttack {
  var isMultiTarget = false
  def apply(target: List[Int], d: Damage, player: PlayerUpdate)
}
object SingleTargetAttack extends RunAttack {
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    if (target.isEmpty) {
      otherPlayer inflict d
    } else {
      val hits = target.count { num ⇒
        val slot = otherPlayer.slots(num)
        val test = slot.value.isDefined
        if (test) {
          slot inflict d
        }
        test
      }
      if (hits < target.size) otherPlayer inflict d
    }
  }

  // BS todo refactor
  // return true if killed a creature
  def attack(target: List[Int], d: Damage, player: PlayerUpdate) = {
    val otherSlots = player.otherPlayer.slots
    val targetExists = target filter { num ⇒ otherSlots(num).value.isDefined }
    apply(target, d, player)
    targetExists.nonEmpty && targetExists.exists(num ⇒ otherSlots(num).value.isEmpty)
  }
}
object MultiTargetAttack extends RunAttack {
  isMultiTarget = true
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val otherPlayer = player.otherPlayer
    otherPlayer inflict d
    otherPlayer.slots inflictCreatures d
  }
}

object Attack {
  def apply(base: Int): AttackSources = AttackSources(Some(base))
}

case class AttackSources(base: Option[Int] = None, sources: Vector[AttackSource] = Vector.empty) {
  def add(source: AttackSource) = copy(sources = sources :+ source)
  def removeFirst(source: AttackSource) = {
    val idx = sources.indexOf(source)
    if (idx != -1) {
      copy(sources = sources.patch(idx, Vector.empty, 1))
    } else this
  }
  def removeFirstEq(source: AttackSource) = {
    val idx = sources.indexWhere(_ eq source)
    if (idx != -1) {
      copy(sources = sources.patch(idx, Vector.empty, 1))
    } else this
  }
  def removeAny(source: AttackSource) = copy(sources = sources.filterNot(_ == source))
}

trait AttackSource
trait AttackFunc extends AttackSource {
  def apply(attack: Int): Int
}
trait AttackStateFunc extends AttackSource {
  def apply(attack: Int, player: PlayerUpdate): Int
}
trait AttackSlotStateFunc extends AttackSource {
  def apply(attack: Int, slot: SlotUpdate): Int
}
case class ManaAttack(houseIndex: Int) extends AttackStateFunc {
  def apply(attack: Int, player: PlayerUpdate): Int = attack + player.getHouses(houseIndex).mana
}

trait DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc]
}
