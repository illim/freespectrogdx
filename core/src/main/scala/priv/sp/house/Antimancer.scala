package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect.Env
import priv.sp.update.{PlayerUpdate, HouseEventListener}
import priv.sp._
import priv.util.FuncDecorators

object Antimancer {

  val angryMob = new Creature("Angry Mob", Attack(3), 9)

  val retaliator = new Creature("Antimancer.Retaliator.name", Attack(5), 10, I18n("Antimancer.Retaliator.description"),
    reaction = new RetaliatorReaction)

  val bombardier = new Creature("Antimancer.Bombardier.name", Attack(5), 34, I18n("Antimancer.Bombardier.description"),
    reaction = new BombardierReaction)

  val Antimancer: House = House("Antimancer", List(

    new Creature("Antimancer.MirrorPriest.name", Attack(2), 12, I18n("Antimancer.MirrorPriest.description"),
      reaction = new MirrorPriestReaction),

    Spell("Antimancer.Resistance.name", I18n("Antimancer.Resistance.description"),
      effects = effects(Direct -> resist)),

    retaliator,

    new Creature("Antimancer.Martyr.name", Attack(4), 17, I18n("Antimancer.Martyr.description"),
      reaction = new MartyrReaction),

    new Creature("Antimancer.Harvester.name", Attack(7), 37, I18n("Antimancer.Harvester.description"),
      reaction = new HarvesterReaction),

    new Creature("Antimancer.VoodooDoll.name", Attack(8), 39, I18n("Antimancer.VoodooDoll.description"),
      effects = effects(OnTurn -> voodoo)),

    bombardier,

    Spell("Antimancer.Bribery.name",  I18n("Antimancer.Bribery.description"),
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
