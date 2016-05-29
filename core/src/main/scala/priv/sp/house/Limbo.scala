package priv.sp.house

import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update.{SlotUpdate, PlayerUpdate, HouseEventListener}
import priv.util.FuncDecorators

import scala.util.Random

object Limbo {
  import CardSpec._

  val heavenKeeper = new Creature("Heaven's Keeper", Attack(3), 7,
    "Each time another owner's creature enters 'Limbo-state' " +
      "Heaven's Keeper heals 2 life to its owner and each of its owner's creatures.",
    reaction = new HeavenKeeperReaction)

  val hellKeeper = new Creature("Hell's Keeper", Attack(3), 10,
    "Each time another owner's creature enters 'Limbo-state' " +
      "Hell's Keeper deals 2 damage to the opponent and each of the opponent's creatures.",
    reaction = new HellKeeperReaction)

  val baron = new Creature("Limbo Baron", Attack(6), 36,
    "when a spell or ability damages and kills KEEPER OF LIMBO or Limbo Baron they enter 'Limbo-state'." +
      " When Limbo Baron enters 'Limbo-state' its attack doubles.")

  val wraith = new Creature("Limbo Wraith", Attack(1) add new WraithAttack, 14,
    "Limbo Wraith's attack is equal to the number of owner's creatures that are in 'Limbo-state' plus 1." +
      "Limbo Wraith's attack damages the opponent and each of the opponent's creatures.",
    runAttack = MultiTargetAttack)

  val Limbo: House = House("Limbo", List(
    heavenKeeper,

    hellKeeper,

    wraith,

    new Creature("Tower of sins", Attack(0), 21,
      "each time an owner's creature enters 'Limbo-state' the opponent's mana of that creature's house is reduced by 1 and" +
        " the owner's mana of that creature's house is increased by 1.",
      reaction = new TowerReaction),

    Spell("Nether Grasp", "destroys each of its caster's creatures and they enter 'Limbo-state'." +
      "then deals 3 damage to each of the opponent's creatures " +
      "for each of the caster's creatures that is in 'Limbo-state'.",
      effects = effects(Direct -> netherGrasp)),

    new Creature("Soul Adjudicator", Attack(3), 21,
      "each time an owner's creature enters 'Limbo-state', a Heaven's Keeper or Hell's Keeper (whichever is in your hand)" +
        "that gains \"can't enter 'Limbo-state'\" restriction is summoned in a random empty slot adjacent to that creature.",
      reaction = new SoulReaction),

    Spell("Redemption", "target caster's creature that is in 'Limbo-state' is re-summoned with full health.",
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> redeem)),

    baron),

    description = "Whenever enemy creature attacks and kills KEEPER OF LIMBO or his creatures," +
      " their death is postponed until the end of KEEPER OF LIMBO's turn, they are considered to be in 'Limbo-state'." +
      " Creatures in 'Limbo-state' attack and block as usual.",
    effects = List(OnEndTurn -> cleanLimbo),
    eventListener = Some(new CustomListener(new LimboEventListener)))

  Limbo initCards Houses.basicCostFunc

  class HeavenKeeperReaction extends Reaction with LimboReaction {
    def onLimbo(s : SlotUpdate): Unit = {
      val p = selected.player
      p heal 2
      p.slots healCreatures 2
      selected.focus()
    }
  }

  class HellKeeperReaction extends Reaction with LimboReaction {
    def onLimbo(s : SlotUpdate): Unit = {
      val p = selected.player.otherPlayer
      p heal 2
      p.slots inflictCreatures Damage(2, Context(selected.playerId, Some(hellKeeper), selected.num), isAbility = true)
      selected.focus()
    }
  }

  class TowerReaction extends Reaction with LimboReaction {
    def onLimbo(s : SlotUpdate): Unit = {
      s.value foreach { slotState =>
        val houseIndex = slotState.card.houseIndex
        selected.player.houses.incrMana(1, houseIndex)
        selected.player.otherPlayer.houses.incrMana(-1, houseIndex)
        selected.focus()
      }
    }
  }

  class SoulReaction extends Reaction with LimboReaction {
    val possibleCards = Set(heavenKeeper, hellKeeper)

    def onLimbo(s : SlotUpdate): Unit = {
      selected.player.pstate.desc.get.houses(4).cards.headOption.foreach { c =>
        c.card match {
          case creature : Creature if possibleCards contains creature =>
            val emptyAdjs = selected.adjacentSlots filter (_.value.isEmpty)

            (Random shuffle emptyAdjs).headOption foreach { s =>
              s add creature
              s setData NoLimbo
              selected.focus()
            }
          case _ =>
        }
      }
    }
  }

  def netherGrasp = { env : Env =>
    val amount = env.player.slots.foldl(0) { (i, s) =>
      s inflict Damage(100, env) // HACK
      i + 3
    }
    env.player.otherPlayer.slots inflictCreatures Damage(amount, env, isSpell = true)
  }

  def redeem = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    if (slot.get.data == LimboState) {
      val card = slot.get.card
      slot.destroy()
      slot add card
    }
  }

  def cleanLimbo = { env : Env =>
    env.player.slots foreach{ s =>
      if (s.get.data == LimboState) {
        s setData NoLimbo
        s inflict Damage(100, env) // !! HACK for phoenix (can't destroy)
      }
    }
    if (env.player.pstate.data == LimboState) {
      env.player setData NoLimbo
      env.player inflict Damage(0, env)
    }
  }

  trait LimboReaction {
    def onLimbo(s : SlotUpdate)
  }

  class LimboEventListener extends HouseEventListener {

    def setLimbo(s : SlotUpdate) = {
      s.write(s.value.map(_.copy(life = 0)))
      s setData LimboState
      player.slots foreach { slot =>
        if (slot.get.card == wraith) {
          slot.attack.setDirty()
        }
        slot.get.reaction match {
          case reaction : LimboReaction => reaction onLimbo s
          case _ =>
        }
      }
    }

    override def onDeath() = {
      player.pstate.data == NoLimbo || {
        if (hasBaron(player)) {
          player setData LimboState
          player write player.pstate.copy(life = 0)
          false
        } else {
          player.pstate.data != LimboState
        }
      }
    }

    def hasBaron( p : PlayerUpdate) = {
      p.slots.slots.exists(s => s.value.isDefined && s.get.card == baron)
    }

    override def init(p: PlayerUpdate) = {
      super.init(p)
      p.slots.slots foreach { slot =>
        slot.delayedDestroy = (FuncDecorators decorate slot.delayedDestroy) update { f =>
          { d: Damage =>
            if (slot.value.isDefined){
              val slotState = slot.get
              if ((slotState.card == baron || !(d.isSpell || d.isAbility))
                && slotState.data != NoLimbo) {
                setLimbo(slot)
                if (slotState.card == baron && slotState.data != LimboState) {
                  slot.attack add DoubleAttackSource
                }
              } else {
                f(d)
              }
            }
          }
        }
        slot.overridableDestroy = (FuncDecorators decorate slot.overridableDestroy) update { f =>
          { () =>
            if (slot.value.isDefined){
              val slotState = slot.get
              if (slotState.card == baron && slotState.data != LimboState) {
                setLimbo(slot)
                slot.attack add DoubleAttackSource
              } else {
                f()
              }
            }
          }
        }
      }
      p.slots.onDead = (FuncDecorators decorate p.slots.onDead) after { dead ⇒
        player.slots foreach { slot =>
          if (slot.get.card == wraith) {
            slot.attack.setDirty()
          }
        }
      }
    }


  }

  class WraithAttack extends AttackStateFunc {

    def apply(attack: Int, player: PlayerUpdate): Int = {
      (attack
      + player.slots.slots.count(s => s.value.isDefined && s.get.data == LimboState)
      + (if (player.pstate.data == LimboState) 1 else 0))
    }
  }

  case object DoubleAttackSource extends AttackSlotStateFunc {
    def apply(attack: Int, slot: SlotUpdate) = {
      attack * 2
    }
  }

  case object LimboState
  case object NoLimbo
}
