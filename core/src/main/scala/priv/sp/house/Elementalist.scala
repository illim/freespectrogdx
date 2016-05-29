package priv.sp.house

import priv.sp._
import priv.sp.update._

class Elementalist {
  import CardSpec._
  import GameCardEffect._

  val Elementalist = House("Elementalist", List(

    new Creature("Sylph", Attack(5), 15, "When enters the game, allows to play additional special card.\n+1 cost for each sylph on the board.",
      reaction = new SylphReaction, effects = effects(Direct -> sylphEffect)),

    Spell("Deep freeze", "Both players skip 1 turn and cannot use special cards in their next turn.",
      effects = effects(Direct -> freeze)),

    new Creature("Salamander", Attack(6), 17, "If owner fire power is higher than opponent fire power, " +
      "deals to opponent 4 damage at the beginning of the turn.", effects = effects(OnTurn -> salamand)),

    Spell("Avalanche", (state : GameState, playerId : PlayerId) =>
      "Deals 2X damage to enemy creatures (X = owner earth power)["+(2 * state.players(playerId).houses(3).mana)+"], " +
        "heals 2X life to owner and reduces owner earth power to 0.",
      effects = effects(Direct -> aval)),

    Spell("Incineration", "Destroys strongest enemy and weakest friendly creatures (calculated by health) both on board and in deck.",
      effects = effects(Direct -> incinerate)),

    new Creature("ArchPhoenix", Attack(9), 20, "Fire cards heal him instead of dealing damage.",
      reaction = new ArchPhoenixReaction),

    new Creature("Stone golem", Attack(7), 30, "Regenerates 4 life when blocked.\nReceives no damage from spells and creatures abilities when unblocked.",
      reaction = new SGReaction, effects = effects(OnTurn -> stoneGole)),

    Spell("Frost lightning", (state : GameState, playerId : PlayerId) =>
      "Deals X damage to opponent\n(X = difference between his lowest power and owner highest power) [" +
        getFrostLightX(state.players(playerId).houses, state.players(other(playerId)).houses) +
      "] and permanently blocks target slot.",
      inputSpec = Some(SelectTargetSlot),
      effects = effects(Direct -> frostLight))))

  val sylph = Elementalist.cards(0)
  Elementalist.initCards(Houses.basicCostFunc)

  val sPhase = "sylph phase"
  def sylphEffect = { env: Env ⇒
    import env._
    player addDescMod IncrSylphCostMod
    player addDescMod HideBasicMod
    player addTransition WaitPlayer(env.playerId, sPhase)
    player addEffectOnce (OnEndTurn -> UnMod(HideBasicMod))
  }

  def freeze = { env: Env ⇒
    env.otherPlayer addDescMod SkipTurn
    env.otherPlayer addEffectOnce (OnEndTurn -> new Unfreeze(true))
  }

  // horror!
  class Unfreeze(chain: Boolean) extends Function[Env, Unit] {
    def apply(env: Env) {
      import env._
      player removeDescMod SkipTurn
      player removeEffect (_.isInstanceOf[Unfreeze])
      player addDescMod HideSpecialMod
      player addEffectOnce (OnEndTurn -> UnMod(HideSpecialMod))
      if (chain) {
        otherPlayer addDescMod SkipTurn
        otherPlayer addEffectOnce (OnEndTurn -> new Unfreeze(false))
      }
    }
  }

  def salamand = { env: Env ⇒
    import env._
    if (player.getHouses(0).mana > otherPlayer.getHouses(0).mana) {
      focus()
      otherPlayer inflict Damage(4, env, isAbility = true)
    }
  }

  def aval = { env: Env ⇒
    import env._
    val x = player.getHouses(3).mana
    otherPlayer.slots inflictCreatures Damage(2 * x, env, isSpell = true)
    player heal (2 * x)
    player.houses.incrMana(-x, 3)
  }

  def incinerate = { env: Env ⇒
    import env._
    def destroy(s: SlotUpdate) {
      val card = s.get.card
      s.overridableDestroy()
      s.slots.player addDescMod Destroyed(card)
    }
    (player.slots reduce lowestLife) foreach destroy
    (otherPlayer.slots reduce highestLife) foreach destroy
  }

  def stoneGole = { env: Env ⇒
    import env._
    if (otherPlayer.getSlots.isDefinedAt(selected)) {
      player.slots(selected) heal 4
    }
  }

  def frostLight = { env: Env ⇒
    import env._
    val x = getFrostLightX(player.getHouses, otherPlayer.getHouses)
    otherPlayer inflict Damage(x, env, isSpell = true)
    otherPlayer blockSlot selected
  }

  def getFrostLightX(houses : PlayerState.HousesType, oppHouses : PlayerState.HousesType) = {
    val opp = oppHouses.reduceLeft((h1, h2) ⇒ if (h1.mana < h2.mana) h1 else h2).mana
    val own = houses.reduceLeft((h1, h2) ⇒ if (h1.mana > h2.mana) h1 else h2).mana
    math.max(0, own - opp)
  }

  class SylphReaction extends Reaction {
    final override def onMyDeath(dead: Dead) {
      dead.player.removeDescMod(IncrSylphCostMod)
    }
  }

  class SGReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      // FiXME: hack watch if unblocked at start of "transaction"! for mass damage and now for titan
      val player = selected.player
      if (!player.updater.value.players(other(player.id)).slots.isDefinedAt(selected.num)
        && !player.otherPlayer.slots(selected.num).value.isDefined) {
        if (d.isEffect) {
          d.copy(amount = 0)
        } else d
      } else d
    }
  }

  class ArchPhoenixReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      d.context.card match {
        case Some(c) if c.houseIndex == 0 ⇒
          selected heal d.amount
          d.copy(amount = 0)
        case _ ⇒ d
      }
    }
  }

  case object IncrSylphCostMod extends DescMod {
    def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
      if (house.houseIndex < 4) cards
      else cards.map { c ⇒
        if (c.card == sylph) {
          c.copy(cost = c.cost + 1)
        } else c
      }
    }
  }
}
