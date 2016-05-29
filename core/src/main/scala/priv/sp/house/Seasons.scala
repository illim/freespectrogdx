package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update.{SlotUpdate, HouseEventListener, PlayerUpdate}
import priv.util.FuncDecorators

object Seasons {

  val initData = SeasonData(midnight = true)
  val defaultData = SeasonData()
  val lowerBasic = LowerHighCostMod(Set(0, 1, 2, 3))

  val nectarBloom = new Creature("Nectarbloom", Attack(2), 10,
    "Nectarbloom regenerates 2 life each turn.",
    effects = effects(OnTurn -> focus(healCreature(2))))
  val ailuv = new Creature("Ailuv Bees", Attack(4), 16,
    "When Ailuv Bees is summoned, two Nectarblooms are summoned into random empty caster’s slots.\n" +
      "Passive: Regrowth: Your mana growth is increased by 1 in each House with fewer than 2 mana.",
    effects = effects(Direct -> spawnNectar))
  val equinox = new Creature("Spirit of Equinox", Attack(5), 12,
    "When summoned, deals 4 damage to opponent and each opponent’s creature, " +
      "and heals 4 life to caster and each caster’s creature.\n" +
      "Passive: Regrowth: Your mana growth is increased by 1 in each House with fewer than 2 mana.",
    effects = effects(Direct -> equinoxEffect))
  val solarWave = Spell("Solar Wave",
    "Opponent takes 2 damage for each of their empty creature slots. Opponent’s creatures take triple damage this turn.\n" +
      "Passive: Midnight Sun: When you take 4 or more damage, that damage is reduced by 1.",
    effects = effects(Direct -> solarWaveEffect))
  val eternal = Spell("Eternal Summer",
    "You may immediately play one creature and one spell from your hand, in either order. Both have -2 cost.\n" +
      "Passive: Midnight Sun: When you take 4 or more damage, that damage is reduced by 1.",
    effects = effects(Direct -> eternalSummerEffect))
  val cornucopia = new Creature("Cornucopia", Attack(0), 26,
    "When summoned, heals all other allied creatures for 9 life. " +
      "All other allied creatures receive +1 attack and take -1 damage while Cornucopia is on the board.\n" +
      "Passive: Plentitude: The cost of your most expensive Fire, Water, Air, and Earth cards is reduced by 1.",
    effects  = effects(Direct -> cornucopiaEffect),
    reaction = new CornucopiaReaction)
  val gaea = new Creature("Gaea s Avatar", AttackSources().add(new GaiaAttack), 60,
    "Attack is equal to number of creatures on the board. " +
      "Gaea's Avatar is unaffected by all spells but takes 1 damage each turn for each empty slot on the board.\n" +
      "Passive: Plentitude: The cost of your most expensive Fire, Water, Air, and Earth cards is reduced by 1.",
    effects  = effects(OnTurn -> gaeaEffect),
    reaction = new GaeaReaction)
  val behemoth = new Creature("Behemoth of Frozen Time", Attack(7), 29,
    "When summoned, opponent’s blocked creatures are stunned for a turn. " +
      "Opponent’s mana in each House cannot grow higher than 9 while Behemoth of Frozen Time is in play.\n" +
      "Passive: Wintry Chill: Opponent’s unblocked creatures take 1 damage each turn.",
    effects  = effects(Direct -> freeze),
    reaction = new BehemothReaction)
  val aurora = Spell("The Great Aurora",
    "Caster’s creatures are healed for 16 health and each grant 3 mana of their own type to their caster.\n" +
      "Passive: Wintry Chill: Opponent’s unblocked creatures take 1 damage each turn.",
    effects = effects(Direct -> greatAuroraEffect))

  val Seasons = House("Seasons",
    List(
      ailuv,
      equinox,
      solarWave,
      eternal,
      cornucopia,
      gaea,
      behemoth,
      aurora),
    data = initData,
    effects = List(
      OnTurn -> new Regrowth,
      OnTurn -> new Wintry),
    description = "“Passing of Seasons”: Each of the Warden’s special cards grants him a small Passive Effect that remains active from the beginning of the game until the card is played.\n" +
      "“Exhaustion”: The Warden’s special cards may only be used once apiece during the match.",
    eventListener = Some(new CustomListener(new SeasonListener)))

  Seasons.initCards(Houses.basicCostFunc)
  Seasons.addAdditionalCards(nectarBloom)


  class Regrowth extends Function[Env, Unit] {
    def apply(env: Env) = {
      env.player.houses.houses.zipWithIndex.foreach { case (house, idx) =>
        if (house.mana < 2) {
          env.player.houses.incrMana(1, idx)
        }
      }
    }
  }

  class Wintry extends Function[Env, Unit] {
    def apply(env : Env) = {
      import env._
      otherPlayer.slots foreach { s =>
        if (player.slots.slots(s.num).value.isEmpty) s inflict Damage(1, env, isAbility = true)
      }
    }
  }

  def freeze = { env : Env =>
    import env._
    otherPlayer.slots foreach { s =>
      if (player.slots.slots(s.num).value.isDefined) s.stun()
    }
  }

  class BehemothReaction extends Reaction {
    val mana9 = new HouseState(9)
    def freezeMana(houseState : HouseState, incr : Int) : HouseState = {
      if (houseState.mana > 9) houseState
      else if (houseState.mana + incr > 9) mana9
      else new HouseState(houseState.mana + incr)
    }
  }

  def gaeaEffect = { env : Env =>
    import env._
    val nbEmpty = player.slots.getOpenSlots.size + otherPlayer.slots.getOpenSlots.size
    getOwnerSelectedSlot() inflict Damage(nbEmpty, env, isAbility = true)
  }

  def cornucopiaEffect = { env : Env =>
    import env._
    player.slots foreach { s =>
      if (s.num != selected) {
        s heal 9
      }
    }
  }

  class CornucopiaReaction extends BullSlotReaction {
    final override def onProtect(d: DamageEvent) = {
      if (d.target.isDefined && d.target != Some(selected.num)) {
        d.damage.copy(amount = math.max(0, d.damage.amount - 1))
      } else d.damage
    }
  }

  def solarWaveEffect = { env : Env =>
    import env._
    otherPlayer inflict Damage(otherPlayer.slots.getOpenSlots.size, env, isSpell = true)
    player.updateData[SeasonData](_.copy(tripleDamage = true))
    player addEffectOnce (OnEndTurn -> RecoverTripeDamage)
  }

  object RecoverTripeDamage extends Function[Env, Unit] {
    def apply(env : Env) = {
      env.player.updateData[SeasonData](_.copy(tripleDamage = false))
    }
  }

  val phase1 = "summer phase 1"
  val phase2 = "summer phase 2"
  def eternalSummerEffect = { env : Env =>
    import env._

    player.updateData[SeasonData](_.copy(summer = true))
    player addTransition WaitPlayer(playerId, phase2)
    player addTransition WaitPlayer(playerId, phase1)

    player addEffectOnce (OnEndTurn -> { env : Env =>
      env.player removeDescMod HideSpellMod
      env.player removeDescMod HideCreatureMod
      player.updateData[SeasonData](_.copy(summer = false))
    })
  }

  def greatAuroraEffect = { env : Env =>
    import env._
    player.slots foreach { slot =>
      slot heal 16
      player.houses.incrMana(3, slot.get.card.houseIndex)
    }
  }

  def equinoxEffect = { env : Env =>
    import env._
    val damage = Damage(4, env, isAbility = true)
    otherPlayer inflict damage
    otherPlayer.slots inflictCreatures damage
    player heal 4
    player.slots healCreatures 4
  }

  def spawnNectar = { env : Env =>
    import env._
    val openSlots = player.slots.getOpenSlots
    if (openSlots.size > 0) {
      val slot = openSlots(player.updater.randLogs get openSlots.size)
      slot add nectarBloom
      val remainings = openSlots.filterNot(_.num == slot.num)
      if (remainings.size > 0) {
        remainings(player.updater.randLogs get remainings.size) add nectarBloom
      }
    }
  }

  class GaeaReaction extends Reaction {
    override def inflict(damage: Damage) {
      if (!damage.isSpell) { super.inflict(damage) }
    }
  }

  class SeasonListener extends HouseEventListener {
    def protect(slot: SlotUpdate, damage: Damage) = {
      player.slots.foldl(damage) { (acc, s) ⇒
        val c = s.get.card
        if (c == cornucopia) {
          s.get.reaction onProtect DamageEvent(acc, Some(slot.num), player)
        } else acc
      }
    }

    def oppDamage(p : PlayerUpdate, slot: SlotUpdate, damage: Damage) = {
      if (getData(p.value).tripleDamage) {
        damage.copy(amount = 3 * damage.amount)
      } else damage
    }

    val regrowthCards   = Set[Card](ailuv, equinox)
    val midnightCards   = Set[Card](solarWave, eternal)
    val plentitudeCards = Set[Card](cornucopia, gaea)
    val wintryCards     = Set[Card](behemoth, aurora)

    def refreshGaea() {
      if (player.getSlots.values.exists(s ⇒ s.card == gaea)) {
        player.slots.filleds.find(s ⇒ s.get.card == gaea) foreach { s ⇒
          s.attack.setDirty()
        }
      }
    }

    def checkSlotChange(slot: SlotUpdate, f: Function[Option[SlotState], Unit]) = { s : Option[SlotState] =>
      val old = slot.value.size
      f(s)
      if (s.size != old) {
        refreshGaea()
      }
    }

    override def init(p: PlayerUpdate): Unit = {
      super.init(p)
      p addDescMod lowerBasic
      p.slots.slots foreach { slot ⇒
        slot.protect modifyResult (d ⇒ protect(slot, d))
        slot.update.update { f =>
          checkSlotChange(slot, f)
        }
      }
      p.otherPlayer.slots.slots foreach { slot =>
        slot.update.update { f =>
          checkSlotChange(slot, f)
        }
      }
      p.submitCommand = (FuncDecorators decorate p.submitCommand) after { c =>
        if (c.card.houseIndex == 4) {
          p addDescMod Destroyed(c.card)
          if (regrowthCards.contains(c.card)) {
            p.removeEffect(_.isInstanceOf[Regrowth])
          } else if (midnightCards.contains(c.card)) {
            player.updateData[SeasonData](_.copy(midnight = false))
          } else if (plentitudeCards.contains(c.card)) {
            p removeDescMod lowerBasic
          } else if (wintryCards.contains(c.card)) {
            p.removeEffect(_.isInstanceOf[Wintry])
          }
        }
        if (getData(p.value).summer && c.card != eternal) {
          p addDescMod (if (c.card.isSpell) HideSpellMod else HideCreatureMod)
        }
      }
      p.guard = (FuncDecorators decorate p.guard) modifyResult { d =>
        if (getData(p.value).midnight && d.amount > 4) {
          d.copy(amount = d.amount - 1)
        } else d
      }
      p.otherPlayer.slots.slots foreach { slot ⇒
        slot.protect modifyResult (d ⇒ oppDamage(player, slot, d))
      }
      p.otherPlayer.houses.addMana = (FuncDecorators decorate p.otherPlayer.houses.addMana) update { f =>
        { case arg @ (houseState, incr) =>
          if (incr > 0) {
            (player.slots.foldl(f) { (acc, slot) =>
              slot.get.reaction match {
                case r : BehemothReaction => { case (houseState, incr) => r.freezeMana(houseState, incr) }
                case _ => acc
              }
            })(arg)
          } else f(arg)
        }
      }
    }
  }

  def getData(p : PlayerState) : SeasonData = {
    p.data match {
      case s : SeasonData => s
      case _ => defaultData
    }
  }

  class GaiaAttack extends AttackStateFunc {
    // can't use player slots as the slots are not yet flushed when calculating the attack
    def apply(attack: Int, player: PlayerUpdate): Int = {
      attack +
        player.slots.slots.count(_.value.isDefined) +
        player.otherPlayer.slots.slots.count(_.value.isDefined)
    }
  }

  case class LowerHighCostMod(indexes : Set[Int]) extends DescMod {
    def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
      if (indexes.contains(house.houseIndex)) {
        cards.lastOption match {
          case Some(c) => cards.take(cards.size - 1) :+ c.copy(cost = math.max(0, c.cost - 1))
          case None => cards
        }
      } else cards
    }
  }
}

case class SeasonData(tripleDamage : Boolean = false, midnight : Boolean = false, summer : Boolean = false)
