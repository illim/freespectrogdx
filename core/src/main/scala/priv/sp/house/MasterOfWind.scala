package priv.sp.house

import priv.sp._
import priv.sp.update._
import GameCardEffect._
import CardSpec._
import priv.util.FuncDecorators

/**
 * Introduced bs: player data
 *
 * TODO custom select for simoom
 */
object MasterOfWind {

  val spirit = new Creature("The spirit of thunderstorm", Attack(2), 28, "gives 1 special mana per turn.\nDeals 4 damage to all enemies when owner skips turn.", reaction = new SpiritThunderReaction, effects = effects(OnTurn -> addMana(1, 4)))

  val Wind: House = House("Wind", List(
    new Creature("Winged Warrior", Attack(4), 9, "next creature summoned will attack on the same turn as summoned.", effects = effects(Direct -> winge)),
    Spell("Simoom", "Stuns target elemental creature, gives X mana\nX = half of cost of that creature (it will be rounded down).",
      inputSpec = Some(SelectTarget(nonSpecial)),
      effects = effects(Direct -> simoom)),
    Spell("Ball Lightning", "Deals to opponent 7 damage, allows to use additional card this turn.", effects = effects(Direct -> ballLightning)),
    Spell("Squall", "Deals 1 damage to opponent and all his creatures.\nAllows to use two additional cards this turn.", effects = effects(Direct -> squall)),
    Spell("Energetic vortex", "increases damage dealt by next owner spell by 50% and transfer 2 air mana from opponent to owner.", effects = effects(Direct -> vortex)),
    spirit,
    Spell("Whirlwind", "destroys the target creature and opposite one,\nallows to use additional card this turn.",
      inputSpec = Some(SelectOwnerCreature),
      effects(Direct -> whirlwind)),
    Spell("Eternal Storm", "this turn all friendly creatures attack directly opponent.", effects = effects(Direct -> storm))),
    eventListener = Some(new CustomListener(new WindEventListener)),
    data = WindState())

  Wind initCards Houses.basicCostFunc

  def winge = { env: Env ⇒
    env.player.updateData[WindState](_.copy(winged = true))
  }

  def simoom = { env: Env ⇒
    import env._

    val slot = otherPlayer.slots(selected)
    val card = slot.get.card
    if (card.houseIndex != 4) {
      slot.stun()
      player.houses.incrMana(card.cost / 2, card.houseIndex)
    }
  }

  val blPhase = "Ball lightning phase"
  def ballLightning = { env: Env ⇒
    import env._
    otherPlayer inflict Damage(7, env, isSpell = true)
    player addTransition WaitPlayer(playerId, blPhase)
  }

  val sqPhase1 = "Squall phase 1"
  val sqPhase2 = "Squall phase 2"
  def squall = { env: Env ⇒
    import env._
    val d = Damage(1, env, isSpell = true)
    otherPlayer inflict d
    otherPlayer.slots inflictCreatures d
    player addTransition WaitPlayer(playerId, sqPhase2)
    player addTransition WaitPlayer(playerId, sqPhase1)
  }

  def vortex = { env: Env ⇒
    import env._
    player.updateData[WindState](_.copy(vortex = true))
    val houses = otherPlayer.houses
    val air = houses.houses(2).mana
    houses.incrMana(-2, 2)
    player.houses.incrMana(math.min(air, 2), 2)
    player addEffect (OnEndTurn -> new CountDown(2, { env: Env ⇒
      env.player.updateData[WindState](_.copy(vortex = false))
    }))
  }

  val wwPhase = "whirlwind phase"
  def whirlwind = { env: Env ⇒
    import env._
    player.slots(selected).destroy()
    otherPlayer.slots(selected).overridableDestroy()
    player addTransition WaitPlayer(playerId, wwPhase)
  }

  def storm = { env: Env ⇒
    import env._
    player.slots foreach { s ⇒
      s write s.value.map(_.copy(target = Nil))
    }
    player addEffectOnce (OnEndTurn -> { env: Env ⇒
      env.player.slots foreach { s ⇒
        s write s.value.map(_.copy(target = List(s.num)))
      }
    })
  }

  class SpiritThunderReaction extends Reaction {
    def onNoCommand = {
      val damage = Damage(3, Context(selected.playerId, Some(spirit), selected.num), isAbility = true)
      val otherPlayer = selected.otherPlayer
      otherPlayer inflict damage
      otherPlayer.slots inflictCreatures damage
    }
  }

  class WindEventListener extends HouseEventListener {
    def onAdd(slot: SlotUpdate) {
      val slotPlayer = slot.player
      if (slotPlayer.id == player.id) {
        val data = player.value.data.asInstanceOf[WindState]
        if (data.winged) {
          slot toggle runFlag
          player.updateData[WindState](_.copy(winged = false))
        }
      }
    }
    override def interceptSubmit(commandOption: Option[Command]): (Boolean, Option[Command]) = {
      if (commandOption.isEmpty) {
        player.slots foreach { s ⇒
          s.get.reaction match {
            case sr: SpiritThunderReaction ⇒ sr.onNoCommand
            case _                         ⇒
          }
        }
      }
      (false, None)
    }

    override def mod(damage: Damage) = {
      val data = player.value.data.asInstanceOf[WindState]
      if (data.vortex) {
        damage.copy(amount = math.ceil(damage.amount * 1.5).intValue)
      } else damage
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.slots.slots foreach (slot ⇒ slot.add = (FuncDecorators decorate slot.add) after(_ ⇒ onAdd(slot)))
    }
  }
}

case class WindState(winged: Boolean = false, vortex: Boolean = false)

