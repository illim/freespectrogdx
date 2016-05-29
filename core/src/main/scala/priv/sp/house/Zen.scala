package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update._

/**
 * Introduced bullshit:
 * electric guard -> added lot of useless check to see if a slot is not killed by guard
 */
class ZenMage {

  private val cocoon = new Creature("Cocoon", Attack(0), 13, "If die by damage, it heals 3 life to owner",
    reaction = new CocoonReaction)
  private val eguard = new Creature("Electric guard", Attack(3), 19, "deals 3 damage to creatures damaging owner.",
    reaction = new EGuardReaction)

  val Zen: House = House("Zen", List(
    new Creature("Elementesist", Attack(3), 12, "Deals damage to opposite card, and to all opposite card of same mana.",
      runAttack = new ElemAttack),
    new Creature("Redlight", Attack(2), 13, "Attack opposite and adjacent slots.",
      runAttack = new RedlightAttack),
    Spell("Focus", (state : GameState, playerId : PlayerId) =>
      "Every owner card dedicate 50% of their attack to heal the focused creature. ["+
        getFocusAmount(state.players(playerId).slots)+"]",
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> focusSpell)),
    eguard,
    new Creature("Dreamer", Attack(6), 24, "When in play spell are summoned with one turn late but with cost -2.",
      reaction = new DreamerReaction),
    new Creature("Mimic", Attack(6), 26, "When in play, creature are summoned with one turn late with cost -2.",
      reaction = new MimicReaction),
    new Creature("Spiral of light", Attack(3), 19, "Each turn, heals by his attack and adjacent cards decreasing when farest.\n" +
      "In the same way, attack opposite creature and adjacent cards.",
      effects = effects(OnTurn -> spiral),
      runAttack = new SpiralAttack),
    new Creature("Zen Fighter", Attack(5), 21, "Increase owner highest mana by 1.\n" +
      "Zen Fighter receives 50% damage from spells and abilities",
      reaction = new ZFReaction,
      effects = effects(OnTurn -> zenEffect))),
    eventListener = Some(new CustomListener(new ZenEventListener)))

  Zen initCards Houses.basicCostFunc
  Zen.addAdditionalCards(cocoon)

  private class RedlightAttack extends RunAttack {
    isMultiTarget = true

    def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
      val num = target.head
      val otherPlayer = player.otherPlayer
      val targets = num - 1 to num + 1

      targets.foreach { n ⇒
        otherPlayer.getSlots.get(n) match {
          case None               ⇒ if (n == num) otherPlayer inflict d
          case Some(oppositeSlot) ⇒ otherPlayer.slots(n) inflict d
        }
      }
    }
  }

  private class SpiralAttack extends RunAttack {
    isMultiTarget = true

    def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
      val num = target.head
      val otherPlayer = player.otherPlayer

      val dist = d.amount - 1
      slotInterval(num - dist, num + dist) foreach { n ⇒
        val damage = d.copy(amount = d.amount - math.abs(num - n))
        val slot = otherPlayer.slots(n)
        if (slot.value.isDefined) slot inflict damage
        else if (n == num) {
          otherPlayer inflict d
        }
      }
    }
  }

  private def focusSpell = { env: Env ⇒
    import env._

    val factor = AttackFactor(0.5f)
    val amount = getFocusAmount(player.slots.value) // Not clean to use the internal value ?
    getOwnerSelectedSlot() heal amount
    player.slots foreach (_.attack add factor)
    player addEffect (OnEndTurn -> new RemoveAttack(factor))
  }

  def getFocusAmount(slots : PlayerState.SlotsType) = {
    slots.foldLeft(0){ case (acc, (_, x)) ⇒ acc + math.ceil(x.attack / 2f).toInt }
  }

  private def spiral = { env: Env ⇒
    import env._

    val attack = getOwnerSelectedSlot().get.attack
    val dist = attack - 1
    slotInterval(selected - dist, selected + dist) foreach { num ⇒
      val amount = attack - math.abs(selected - num)
      val slot = player.slots(num)
      if (slot.value.isDefined) slot heal amount
    }
  }

  private def zenEffect = { env: Env ⇒
    import env._
    val houseIndex = player.getHouses.zipWithIndex.maxBy(_._1.mana)._2
    player.houses.incrMana(1, houseIndex)
  }

  private class EGuardReaction extends Reaction {
    final override def onProtect(d: DamageEvent) = {
      import d._
      if (target.isEmpty) {
        damage.context.selectedOption foreach { num ⇒
          player.updater.focus(selected.num, player.id, blocking = false)
          player.updater.players(damage.context.playerId).slots(num) inflict Damage(3, Context(player.id, Some(eguard), selected.num), isAbility = true)
        }
      }
      d.damage
    }
  }

  trait ZenReaction {
    def interceptSubmit(command: Command, updater: GameStateUpdater): (Boolean, Option[Command])
  }

  private class DreamerReaction extends Reaction with ZenReaction {
    final def interceptSubmit(command: Command, updater: GameStateUpdater) = {
      if (command.card.isSpell && command.flag == None) {
        val c = command.copy(flag = Some(DreamCommandFlag), cost = math.max(0, command.cost - 2))
        updater.players(command.player) addEffectOnce (OnTurn -> new Dream(c))
        (true, None)
      } else (false, None)
    }
  }

  private class MimicReaction extends Reaction with ZenReaction {
    final def interceptSubmit(command: Command, updater: GameStateUpdater) = {
      if (!command.card.isSpell && command.flag == None) {
        val c = command.copy(flag = Some(DreamCommandFlag), cost = math.max(0, command.cost - 2))
        updater.players(command.player) addEffectOnce (OnTurn -> new Hatch(c))
        (true, Some(Command(command.player, cocoon, command.input, 0)))
      } else (false, None)
    }
  }

  private class Hatch(c: Command) extends Function[Env, Unit] {
    def apply(env: Env) {
      if (c.card.inputSpec.exists {
        case SelectOwnerSlot ⇒
          env.player.slots().get(c.input.get.num).exists(_.card == cocoon)
        case _ ⇒ false
      }) {
        env.player submit Some(c)
      }
    }
  }

  class CocoonReaction extends Reaction {
    final override def onMyDeath(dead: Dead) {
      if (dead.damage.isDefined) {
        dead.player.heal(3)
      }
    }
  }

  private class Dream(c: Command) extends Function[Env, Unit] {
    def apply(env: Env) {
      if (!c.card.inputSpec.exists {
        case SelectOwner(f)  ⇒
          val slots = f(env.playerId, env.updater.state)
          slots contains c.input.get.num
        case SelectTarget(_) ⇒ sys.error("not managed!!!!")
        case SelectOwnerSlot ⇒
          env.player.slots() isDefinedAt c.input.get.num
        case SelectOwnerCreature ⇒
          !env.player.slots().isDefinedAt(c.input.get.num)
        case SelectTargetSlot ⇒
          env.otherPlayer.slots() isDefinedAt c.input.get.num
        case SelectTargetCreature ⇒
          !env.otherPlayer.slots().isDefinedAt(c.input.get.num)
      }) {
        env.player submit Some(c)
      }
    }
  }

  class ZFReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      if (d.isEffect) d.copy(amount = math.ceil(0.5 * (d.amount)).intValue)
      else d
    }
  }

  class ZenEventListener extends HouseEventListener {
    override def interceptSubmit(commandOption: Option[Command]): (Boolean, Option[Command]) = {
      commandOption match {
        case Some(c) if (c.player == player.id) ⇒
          player.slots.foldl((false, Option.empty[Command])) { (acc, s) ⇒
            if (acc._1) acc else {
              s.get.reaction match {
                case z: ZenReaction ⇒
                  z.interceptSubmit(c, player.updater)
                case _ ⇒ acc
              }
            }
          }
        case _ ⇒ (false, None)
      }
    }
  }
}

object DreamCommandFlag extends CommandFlag

class ElemAttack extends RunAttack {
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    (otherPlayer.getSlots get num) match {
      case None ⇒ otherPlayer inflict d
      case Some(oppositeSlot) ⇒
        val h = oppositeSlot.card.houseIndex
        otherPlayer.slots foreach { s ⇒
          if (s.get.card.houseIndex == h) {
            s inflict d
          }
        }
    }
  }
}
