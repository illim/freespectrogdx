package priv.sp.house

import priv.sp._
import priv.sp.update._

object LostChurch {
  import CardSpec._
  import GameCardEffect._

  val liberatorLife = 15

  val prisoner = new Creature("disciples.prisoner", Attack(2), 10, I18n("disciples.prisoner.description"), reaction = new PrisonerReaction, runAttack = new PrisonerAttack)
  val enragedPrisoner = new Creature("disciples.enraged", Attack(6), 35, I18n("disciples.enraged.description"), reaction = new PrisonerReaction, status = runFlag, runAttack = new PrisonerAttack)
  val windOfOppression = Spell("disciples.wind", I18n("disciples.wind.description"), effects = effects(Direct -> oppress))
  val darkMonk = new Creature("disciples.monk", Attack(2), 13, I18n("disciples.monk.description"),
    effects = effects(Direct -> guardFire), reaction = new DarkMonkReaction)
  val preacher = new Creature("disciples.preacher", Attack(4), 13, I18n("disciples.preacher.description"),
    effects = effects(OnTurn -> addMana(1, 4)), reaction = new PreacherReaction)
  val falseProphet : Creature = new Creature("disciples.prophet", Attack(4), 14, I18n("disciples.prophet.description"),
    reaction = new FalseProphetReaction, effects = effects(Direct -> prophetize))
  val astralEscape = new Creature("disciples.escape", Attack(4), 30, I18n("disciples.escape.description"), reaction = new AstralEscapeReaction)
  val scarecrow : Creature = new Creature("disciples.scarecrow", Attack(7), 25,
    I18n("disciples.scarecrow.description"),
    effects = effects(Direct -> scare), inputSpec = Some(SelectOwner(openOrPrisoner)), reaction = new ScarecrowReaction)
  val liberator = new Creature("disciples.liberator", Attack(3), liberatorLife, I18n.bundle.format("disciples.liberator.description", liberatorLife.toString), reaction = new LiberatorReaction, effects = effects(Direct -> focus(deliverPrisoner)))

  val LostChurch = new House("disciples", List(
    Spell("disciples.drug", I18n("disciples.drug.description"),
      effects = effects(Direct -> speedDrug)),
    preacher,
    falseProphet,
    astralEscape,
    scarecrow,
    liberator,
    new Creature("disciples.falconer", Attack(6), 30, I18n("disciples.falconer.description"), effects = effects(OnTurn -> focus(falcon))),
    Spell("disciples.madden", I18n("disciples.madden.description"), effects = effects(Direct -> madden))),
    effects = List(OnEndTurn -> spawnPrisoner, OnTurn -> weaken),
    eventListener = Some(new CustomListener(new LCEventListener)),
    description = I18n("disciples.description"))

  LostChurch initCards Houses.basicCostFunc
  LostChurch.addAdditionalCards(prisoner, enragedPrisoner, windOfOppression, darkMonk)
  windOfOppression.cost = 3
  windOfOppression.cardIndex = 4
  darkMonk.cost = 3
  darkMonk.cardIndex = 3
  prisoner.cost = 1
  enragedPrisoner.cost = 4

  val falseProphetAbility = Ability(falseProphet, darkMonk)
  val scarecrowAbility = Ability(scarecrow, windOfOppression)

  def openOrPrisoner(playerId: PlayerId, state: GameState): List[Int] = {
    val p = state.players(playerId)
    if (p.slots.size == p.slotList.size) Nil
    else {
      p.slotList.foldLeft(List.empty[Int]) {
        case (acc, i) ⇒
          p.slots.get(i) match {
            case None                          ⇒ i :: acc
            case Some(s) if s.card == prisoner ⇒ i :: acc
            case _                             ⇒ acc
          }
      }
    }
  }

  def spawnPrisoner: Effect = { env: Env ⇒
    import env._
    if (!player.slots().exists { case (n, slot) ⇒ slot.card == prisoner || slot.card == enragedPrisoner }) {
      val openSlots = player.slots.getOpenSlots
      if (openSlots.nonEmpty) {
        val slot = openSlots(updater.randLogs get openSlots.size)
        slot add prisoner
        if ((player.slots findCard preacher).isDefined) {
          slot.attack add PreacherAttackBonus
        }
        slot.focus(blocking = false)
      }
    }
  }
  def guardFire = { env: Env ⇒
    env.otherPlayer.houses.incrMana(-2, 0)
  }
  def weaken: Effect = { env: Env ⇒
    import env._
    player.slots() foreach {
      case (num, slot) ⇒
        if (slot.card.houseId == LostChurch.houseId && slot.life < (slot.card.life / 2) && !slot.attackSources.sources.exists(_.isInstanceOf[LCAttack])) {
          player.slots(num).attack add LCAttack(-math.ceil(slot.card.attack.base.get / 3f).toInt)
        }
    }
  }

  def getPrisonerSlot(slots: SlotsUpdate): Option[SlotUpdate] = {
    slots.slots find { slot ⇒ slot.value.isDefined && (slot.get.card == prisoner || slot.get.card == enragedPrisoner) }
  }

  def giveHope(player: PlayerUpdate) = {
    getPrisonerSlot(player.slots) foreach { slot ⇒
      if (!slot.attack.has[PreacherAttackBonus.type]) {
        slot.attack add PreacherAttackBonus
      }
    }
  }
  class PreacherReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) = {
      if (selected.num == slot.num) {
        val player = selected.slots.player
        player addDescMod IncrBasicCostMod
        giveHope(player)
      }
    }
    final override def cleanUp() = {
      val slots = selected.slots
      getPrisonerSlot(slots) foreach { slot ⇒
        if ((slots findCard preacher).isEmpty) {
          slot.attack removeAny PreacherAttackBonus
        }
      }
      slots.player removeDescMod IncrBasicCostMod
    }
  }
  def prophetize = { env: Env ⇒
    import env._
    player.houses.incrMana(2, 0, 1, 2, 3)
    player.addDescMod(IncrBasicCostMod, falseProphetAbility)
  }

  def scare = { env: Env ⇒
    val slot = env.otherPlayer.slots(env.selected)
    if (slot.value.isDefined) {
      env.focus()
      slot inflict Damage(4, env, isAbility = true)
      slot.stun()
    }
    env.player addDescMod scarecrowAbility
  }
  def oppress = { env: Env ⇒
    import env._
    (player.slots findCard scarecrow) foreach { slot ⇒
      slotInterval(slot.num - 1, slot.num + 1) foreach { n ⇒
        val oppSlot = otherPlayer.slots(n)
        if (oppSlot.value.isDefined) {
          oppSlot inflict Damage(4, env, isAbility = true)
          oppSlot toggle stunFlag
        }
      }
    }
  }
  class ScarecrowReaction extends Reaction {
    override def onSpawnOver = {
      selected.value foreach { old ⇒
        nearestEmptySlot(selected.num, selected.player) foreach { n ⇒
          selected.slots.move(selected.num, n)
        }
      }
      None
    }

    final override def cleanUp() {
      selected.player removeDescMod scarecrowAbility
    }
  }

  def deliverPrisoner = { env: Env ⇒
    import env._
    getPrisonerSlot(player.slots) foreach {
      case slot ⇒
        slot.destroy()
        slot add enragedPrisoner
    }
  }

  def speedDrug = { env: Env ⇒
    import env._
    player.slots foreach (_.attack add LCAttackBonus(env.player.id))
    player.slots inflictCreatures Damage(4, env, isSpell = true)
  }

  val maddenBonus = AttackAdd(1)
  def madden = { env: Env ⇒
    import env._
    otherPlayer.slots foreach { slot ⇒
      val d = Damage(8, env, isSpell = true)
      slot.inflict(d)
      if (slot.value.isDefined) {
        slot.attack add maddenBonus
      } else {
        player heal 3
      }
    }
    player.slots foreach { _.attack add maddenBonus }
  }
  def falcon = { env: Env ⇒
    import env._
    focus()
    otherPlayer.slots foreach { slot ⇒
      if (slot.num != selected) {
        slot inflict Damage(math.abs(slot.num - selected), env, isAbility = true)
      }
    }
  }

  class LiberatorReaction extends Reaction {
    final override def onMyDeath(dead: Dead) = {
      (dead.player.slots findCard enragedPrisoner) foreach { slot ⇒
        slot inflict Damage(liberatorLife, Context(dead.player.id))
      }
    }

    final override def onProtect(d: DamageEvent) = {
      d.target match { // hack
        case Some(n) if selected.num != n && d.player.slots(n).get.card == enragedPrisoner && d.damage.isEffect ⇒
          d.damage.copy(amount = 0)
        case _ ⇒ d.damage
      }
    }
  }

  class PrisonerReaction extends Reaction {
    final override def onMyDeath(dead: Dead) = {
      import dead.player
      if ((player.slots findCard liberator).isEmpty) {
        val houses = player.houses.houses
        val highs = (0 to 3) sortBy { i ⇒ houses(i).mana } drop 2
        player.houses.incrMana(-1, highs: _*)
        player.houses.incrMana(1, 4)
      }
      val bonus = LCAttackBonus(dead.player.id)
      player.slots foreach { _.attack removeAny bonus }
    }
  }

  // crap
  class LCEventListener extends HouseEventListener {
    def protect(slot: SlotUpdate, damage: Damage) = {
      val target = slot.get.card
      if (target == prisoner || target == enragedPrisoner) {
        player.slots.foldl(damage) { (acc, s) ⇒
          val sc = s.get.card
          if (sc == astralEscape || sc == liberator) {
            s.get.reaction onProtect DamageEvent(acc, Some(slot.num), player)
          } else acc
        }
      } else damage
    }

    override def init(p: PlayerUpdate) = {
      super.init(p)
      p.slots.slots foreach { slot ⇒
        slot.protect modifyResult (d ⇒ protect(slot, d))
      }
    }
  }

  class AstralEscapeReaction extends Reaction {
    final override def onProtect(d: DamageEvent) = {
      import d._
      var res = d.damage
      if (target.isDefined) {
        val slot = player.slots(d.target.get)
        if (slot.get.card == prisoner || slot.get.card == enragedPrisoner) {
          selected inflict d.damage
          res = d.damage.copy(amount = 0)
        }
      }
      res
    }
  }

  class PrisonerAttack extends RunAttack {

    def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
      val num = target.head
      val otherPlayer = player.otherPlayer
      val slot = otherPlayer.slots(num)
      val escaped = player.slots.filleds.exists(_.get.reaction.isInstanceOf[AstralEscapeReaction])
      if (escaped) {
        otherPlayer inflict d
      } else {
        if (slot.value.isDefined) {
          slot inflict d
        } else {
          otherPlayer inflict d
        }
      }
    }
  }
}


class DarkMonkReaction extends Reaction {
  final override def onAdd(slot: SlotUpdate) = {
    if (selected.num == slot.num) {
      slot.otherPlayer addDescMod IncrFireCostMod
    }
  }
  final override def onMyRemove(dead: Option[Dead]) = {
    selected.otherPlayer removeDescMod IncrFireCostMod
  }
}
object PreacherAttackBonus extends AttackAdd(1) with UniqueAttack
case class LCAttack(half: Int) extends AttackFunc { def apply(attack: Int) = math.max(0, attack + half) }
case object IncrBasicCostMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex == 4) cards
    else cards.map(c ⇒ c.copy(cost = c.cost + 1))
  }
}
case object IncrFireCostMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex == 0) cards.map(c ⇒ c.copy(cost = c.cost + 1))
    else cards
  }
}

case class LCAttackBonus(player: PlayerId) extends AttackFunc { def apply(attack: Int) = attack + 1 }

class FalseProphetReaction extends Reaction {
  final override def onMyDeath(dead: Dead) = {
    dead.player.houses.incrMana(-1, 0, 1, 2, 3)
  }
  final override def cleanUp() = {
    selected.player removeDescMod IncrBasicCostMod
  }
}

