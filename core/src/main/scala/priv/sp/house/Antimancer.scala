package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect.Env
import priv.sp.update.{PlayerUpdate, HouseEventListener}
import priv.sp._
import priv.util.FuncDecorators

object Antimancer {

  val angryMob = new Creature("Angry Mob", Attack(3), 9)

  val retaliator = new Creature("Retaliator", Attack(5), 10,
    "Every time retaliator looses life it deals an equivalent amount of damage to all opponent's creatures.",
    reaction = new RetaliatorReaction)

  val bombardier = new Creature("Bombardier", Attack(5), 34,
    "When Bombadier deals damage to opponent it deals the same amount of damage to all opponent's cards.\n" +
      "When Bombadier dies it deals 28 damage to the card in the opposing slot.",
    reaction = new BombardierReaction)

  val Antimancer: House = House("Antimancer", List(

    new Creature("Mirror Priest", Attack(2), 12,
      "Each time opponent plays a card gain 1 power of that card's type. If opponent played a special card, gain 1 Revenge.",
      reaction = new MirrorPriestReaction),

    Spell("Resistance", "Put a 3/9 card into each of your unblocked slots.",
      effects = effects(Direct -> resist)),

    retaliator,

    new Creature("Martyr", Attack(4), 17,
      "As long as Martyr is in play all your other cards that die or get destroyed by a spell will rebirth.",
      reaction = new MartyrReaction),

    new Creature("Harvester", Attack(7), 37,
      "Each time any of owner's creatures die Harvester randomly increases its owner's fire, water, air or earth power by 1.",
      reaction = new HarvesterReaction),

    new Creature("Voodoo Doll", Attack(8), 39,
      "At the beginning of its owner's turn Voodoo Doll drains 1 life from each of opponent's cards.\n"
      +"Drain life means each opponent card looses 1 life and player gains life equal to the amount drained.",
      effects = effects(OnTurn -> voodoo)),

    bombardier,

    Spell("Bribery", "Destroy target opponent's card with power cost 7 or less.\n" +
      "That card will reappear in one of owner's random empty slots with maximum life.",
      inputSpec = Some(SelectTarget(cost7OrInf)),
      effects = effects(Direct -> bribe))
  ),
  eventListener = Some(new CustomListener(new AntimancerListener)))

  Antimancer initCards { i: Int ⇒ i + 2 }
  Antimancer addAdditionalCards angryMob

  def cost7OrInf(p: PlayerId, state: GameState): List[Int] = {
    state.players(p).slots.foldLeft(List.empty[Int]) {
      case (acc, (i, s)) ⇒
        if (s.card.cost < 8) i :: acc else acc
    }
  }

  def resist = { env : Env =>
    def spawnCreature(num: Int) : Unit = {
      val slot = env.player.slots(num)
      if (slot.value.isEmpty && slot.oppositeSlot.value.isEmpty) {
        slot add angryMob
      }
    }
    env.player.value.slotList foreach spawnCreature
  }

  def bribe = { env : Env =>
    env.updater.randLogs.unorderedShuffle(env.player.slots.getOpenSlots).headOption foreach { s =>
      val oppSlot = env.otherPlayer.slots(env.selected)
      val card = oppSlot.get.card
      oppSlot.destroy()
      s add card
    }
  }

  def voodoo = { env : Env =>
    val d = Damage(1, env, isAbility = true)
    val filleds = env.otherPlayer.slots.filleds
    filleds foreach { slot => slot drain d }
    env.player heal filleds.size
  }

  class RetaliatorReaction extends Reaction {
    final override def onMyDamage(damage: Damage) {
      selected.player.otherPlayer.slots inflictCreatures Damage(damage.amount, Context(selected.playerId, Some(retaliator), selected.num), isAbility = true)
      selected.focus()
    }
  }

  class MirrorPriestReaction extends Reaction {

    final def onSubmit(c : Command) = {
      selected.player.houses.incrMana(1, c.card.houseIndex)
      selected.focus()
    }
  }

  class MartyrReaction extends Reaction {

    final override def onDeath(dead: Dead) : Unit = {
      if (dead.isSpell || dead.isDestroy) {
        selected.player.slots(dead.num) add dead.card
        selected.focus()
      }
    }
  }

  class HarvesterReaction extends Reaction {

    final override def onDeath(dead: Dead) : Unit = {
      selected.player.houses.incrMana(1, selected.updater.randLogs get 4)
      selected.focus()
    }
  }

  class BombardierReaction extends Reaction {
    final def onPlayerDamage(damage: Damage) = {
      if (damage.context.selected == selected.num && damage.context.playerId == selected.playerId) {
        selected.otherPlayer.slots inflictCreatures damage
        selected.focus()
      }
    }
    final override def onMyDeath(dead: Dead) {
      selected.oppositeSlot inflict Damage(28, Context(selected.playerId, Some(bombardier), selected.num), isAbility = true)
    }
  }


  class AntimancerListener extends HouseEventListener with OwnerDeathEventListener {

    final def onPlayerDamage(damage: Damage) = {
      player.slots foreach { s =>
        s.get.reaction match {
          case r: BombardierReaction ⇒ r onPlayerDamage damage
          case _ ⇒
        }
      }
    }

    final override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.submitCommand = (FuncDecorators decorate p.otherPlayer.submitCommand) after { c ⇒
        player.slots foreach { s ⇒
          s.get.reaction match {
            case r: MirrorPriestReaction ⇒ r onSubmit c
            case _ ⇒
          }
        }
      }
      p.otherPlayer.onPlayerDamage = (FuncDecorators decorate p.otherPlayer.onPlayerDamage) after { d: Damage =>
        onPlayerDamage(d)
      }
    }
  }

}
