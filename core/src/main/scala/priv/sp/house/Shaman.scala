package priv.sp.house

import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update._
import priv.util.FuncDecorators

import scala.collection._

object Shaman {
  import CardSpec._

  val initState = WolfState()
  val wolf = new Creature("ghost wolf", AttackSources(Some(2)).add(new WolfAttackBonus), 18, reaction = new WolfReaction, runAttack = new WolfAttack,
    effects = effects(Direct -> wolfSummoned))
  val shadow = new Creature("Wolf shadow", AttackSources(Some(4)).add(new ShadowAttack), 45, "all cards which affect wolf affect wolf shadow as well.\nWhen enters the game, its neighbours attack immediately.",
    reaction = new WolfShadowReaction,
    effects = effects(Direct -> shade),
    runAttack = new WolfAttack)
  val protector = new Creature("Spirit protector", Attack(4), 20, "while protector remains in the game, all damage received by owner will be decreased by 2, and enemy spells will heal wolf instead of damaging.", reaction = new ProtectorReaction)

  val Shaman: House = House("Shaman", List(
    Spell("Unappeasable Hunger", "wolf receives +X attack (X - attack of strongest creature on board) for 1 turn.",
      effects = effects(Direct -> hunger)),
    new Creature("Spirit of rage", Attack(2), 10, "when enters the game, permanently increases attack of neighbours by 1.", effects = effects(Direct -> rage)),
    Spell("Power of full moon", "permanently decreases damage dealt to wolf by 1 and heals 8 life to his neighbours and owner.", effects = effects(Direct -> fullMoon)),
    Spell("Phantom fury", "Deals 7 damage to all enemy creatures and permanently increases wolf attack by 1 for each creature died this turn.", effects = effects(Direct -> phantomFury)),
    protector,
    new Creature("Spirit hunter", Attack(6), 34, "while hunter remains in the game, wolf gets +2 attack and heals himself on the dealt damage when attacks.",
      reaction = new HunterReaction,
      effects = effects(Direct -> hunt)),
    shadow,
    new Creature("Phantom mate", Attack(5), 29, "when enters the game, permanently decreases cost of wolf cards by 1.\nEvery turn wolf additionally attacks slot opposite to mate.",
      reaction = new MateReaction,
      effects = effects(Direct -> mate))),
    effects = List(OnStart -> initWolf),
    data = initState,
    eventListener = Some(new CustomListener(new ShamanEventListener)),
    description = "Spirit of ancestors:\nAt the beginning of the game ghost wolf appears in slot next to the most right.")

  Shaman.addAdditionalCards(wolf)
  wolf.cost = 2
  Shaman initCards Houses.basicCostFunc

  def getData(p: PlayerUpdate) = {
    p.value.data match {
      case w : WolfState => w
      case _ => initState
    }
  }

  def initWolf = { env: Env ⇒
    val openSlots = env.player.slots.getOpenSlots.take(5)
    val slot = openSlots.last
    slot add wolf
    slot.focus(blocking = false)
  }

  def wolfSummoned = { env: Env ⇒
    import env._
    player removeDescMod WolfMod
    getData(player).shadows foreach { n ⇒
      player.slots(n).attack.setDirty()
    }
  }

  class WolfAttackBonus extends AttackStateFunc {
    def apply(attack: Int, player: PlayerUpdate): Int = {
      attack + getData(player).attackBonus
    }
  }

  class ShadowAttack extends AttackStateFunc {
    def apply(attack: Int, player: PlayerUpdate): Int = {
      player.slots findCard wolf match {
        case Some(s) ⇒ attack + s().get.attack - 2
        case None    ⇒ attack
      }
    }
  }

  def getMaxAttack(player: PlayerUpdate, default: Int = 0) = {
    player.getSlots.foldLeft(default) {
      case (acc, (_, s)) ⇒
        if (s.attack > acc) s.attack else acc
    }
  }

  def hunger = { env: Env ⇒
    import env._
    var maxAttack = getMaxAttack(player)
    maxAttack = getMaxAttack(player.otherPlayer, maxAttack)
    player.slots.findCard(wolf) foreach { s ⇒
      val bonus = AttackAdd(maxAttack)
      s.attack add bonus
      player addEffectOnce (OnEndTurn -> new RemoveAttack(bonus))
    }
  }

  def rage = { env: Env ⇒
    env.getOwnerSelectedSlot.filledAdjacents foreach { s ⇒
      env.player.updateData[WolfState](x ⇒ x.copy(enhanceds = x.enhanceds + (s.get.card.id -> (x.enhanceds(s.get.card.id) + 1))))
      s.attack add OneAttackBonus
    }
  }

  // BS because shadow plugged on attack update
  def fakeUpdate(a: AttackUpdate) = a.write(a.value)

  def fullMoon = { env: Env ⇒
    import env._
    player.updateData[WolfState](x ⇒ x.copy(protection = x.protection + 1))
    player.slots.findCard(wolf) foreach (_.filledAdjacents.foreach(_.heal(8)))
    player heal 8
  }

  def phantomFury = { env: Env ⇒
    import env._
    val damage = Damage(7, Context(env.playerId, None), isSpell = true)
    otherPlayer.slots inflictCreatures damage
    env.player.updateData[WolfState](_.copy(furyWolf = player.slots.findCard(wolf).map(_.num)))
    player addEffectOnce (OnEndTurn -> new RemoveFury)
  }

  val huntBonus = AttackAdd(2)
  def hunt = { env: Env ⇒
    env.player.updateData[WolfState](x ⇒ x.copy(hunting = x.hunting + 1))
    (env.player.slots findCard wolf) foreach (_.attack.add(huntBonus))
  }

  def shade = { env: Env ⇒
    import env._
    player.updateData[WolfState](x ⇒ x.copy(shadows = x.shadows + env.selected))
    getOwnerSelectedSlot.filledAdjacents foreach { slot ⇒
      player runSlot slot
    }
  }

  def mate = { env: Env ⇒
    env.player.updateData[WolfState](x ⇒ x.copy(mates = x.mates + env.selected))
    env.player addDescMod DecrSpecialCostMod
  }

  class HunterReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) = {
      if (slot.get.card == wolf) {
        slot.attack add huntBonus
      }
    }

    final override def onRemove(slot: SlotUpdate) {
      if (slot.get.card == wolf) {
        slot.attack removeFirst huntBonus
      }
    }
    final override def onMyRemove(dead: Option[Dead]) = {
      selected.slots.findCard(wolf).foreach { s ⇒
        s.attack removeFirst huntBonus
      }
      dead foreach (_.player.updateData[WolfState](x ⇒ x.copy(hunting = x.hunting - 1)))
    }
  }

  class ProtectorReaction extends Reaction {
    override def onProtect(d: DamageEvent) = {
      import d._
      if (target.isEmpty) {
        damage.copy(amount = math.max(0, damage.amount - 2).intValue)
      } else {
        if (d.damage.isSpell) {
          val slot = selected.slots(d.target.get)
          slot.value match {
            case Some(s) if s.card == wolf ⇒
              selected.focus(blocking = false)
              slot heal d.damage.amount
              d.damage.copy(amount = 0)
            case _ ⇒ d.damage
          }
        } else d.damage
      }
    }
  }

  class CommonWolf extends Reaction {
    override def selfProtect(d: Damage) = {
      val wolfState = getData(selected.player)
      if (wolfState.protection != 0) {
        d.copy(amount = math.max(0, d.amount - wolfState.protection))
      } else d
    }
  }

  class WolfShadowReaction extends CommonWolf {
    override def cleanUp() {
      selected.player.updateData[WolfState](x ⇒ x.copy(shadows = x.shadows - selected.num))
    }
  }

  // BS
  class WolfReaction extends CommonWolf {
    def shadows = getData(selected.player).shadows.map(x ⇒ selected.slots(x))
    override def heal(amount: Int) {
      super.heal(amount)
      shadows foreach (_.heal(amount))
    }
    override def inflict(damage: Damage) {
      super.inflict(damage)
      shadows foreach (_.inflict(damage))
    }
    override def destroy() {
      super.destroy()
      shadows foreach (_.destroy())
    }
    override def stun() {
      super.stun()
      shadows foreach { _.stun() }
    }
    override def cleanUp() {
      selected.player insertDescMod WolfMod
      shadows foreach { _.attack.setDirty() }
    }
  }

  val wolfDesc = CardDesc(wolf, 2, true)
  case object WolfMod extends DescMod {
    def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
      if (house.houseIndex < 4) cards
      else wolfDesc +: cards.drop(1)
    }
  }

  private class WolfAttack extends RunAttack with DamageAttack {

    def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
      val wolfState = getData(player)
      val num = target.head
      var healAmount = damageAndGet(num, d, player)
      if (wolfState.mates.size > 0) {
        wolfState.mates.foreach { n ⇒
          healAmount += damageCreatureAndGet(n, d, player)
        }
      }
      if (wolfState.hunting > 0) {
        player.slots(num) heal healAmount
      }
    }
  }

  class ShamanEventListener extends HouseEventListener {
    def protect(slot: SlotUpdate, damage: Damage) = {
      player.slots.foldl(damage) { (acc, s) ⇒
        val sc = s.get.card
        if (sc == protector) {
          s.get.reaction onProtect DamageEvent(acc, Some(slot.num), player)
        } else acc
      }
    }
    def reactDead(dead: Dead) {
      if (dead.player.id != player.id) {
        val furyWolf = getData(player).furyWolf
        if (furyWolf.nonEmpty) {
          player.updateData[WolfState](x ⇒ x.copy(attackBonus = x.attackBonus + 1))
          furyWolf foreach { n ⇒
            player.slots(n).attack.setDirty()
          }
        }
      }
    }
    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.slots.onDead = (FuncDecorators decorate p.otherPlayer.slots.onDead) after reactDead
      val slots = p.slots.slots // ! empty includeds
      slots.foreach { s ⇒
        s.attackUpdate.update after { _ ⇒
          if (s.value.isDefined && s.get.card == wolf) {
            getData(p).shadows foreach { n ⇒
              slots(n).attack.setDirty()
            }
          }
        }
      }
      p.slots.slots.foreach { slot ⇒
        slot.protect.modifyResult(d ⇒ protect(slot, d))
      }
      p.submitCommand = (FuncDecorators decorate p.submitCommand)  after { c ⇒
        c.input foreach { input ⇒
          getData(p).enhanceds.get(c.card.id) foreach { bonus ⇒
            val slot = p.slots(input.num)
            if (slot.value.isDefined) slot.attack add AttackAdd(bonus)
          }
        }
      }
    }
  }
}

case class WolfState(
  protection: Int = 0,
  furyWolf: Option[Int] = None,
  attackBonus: Int = 0,
  hunting: Int = 0,
  mates: Set[Int] = Set.empty,
  shadows: Set[Int] = Set.empty,
  enhanceds: immutable.Map[Int, Int] = immutable.Map.empty.withDefault(_ ⇒ 0))

class MateReaction extends Reaction {

  override def cleanUp() {
    selected.player.updateData[WolfState](x ⇒ x.copy(mates = x.mates - selected.num))
  }
}
class RemoveFury extends Function[Env, Unit] {
  def apply(env: Env) {
    env.player.updateData[WolfState](_.copy(furyWolf = None))
  }
}

case object DecrSpecialCostMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex < 4) cards
    else cards.map(c ⇒ c.copy(cost = math.max(0, c.cost - 1)))
  }
}

