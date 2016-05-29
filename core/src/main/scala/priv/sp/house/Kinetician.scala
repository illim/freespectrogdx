package priv.sp.house

import priv.sp._
import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp.update.{SlotUpdate, PlayerUpdate}
import priv.util.FuncDecorators

object Kinetician extends ChangeTarget {

  val manipulator = new Creature("Manipulator", Attack(3), 28,
    "when enters the game, attack of all enemy creatures becomes equal to 3 for 1 turn." +
      "When opponent summons creature, it switches owners, manipulator loses 4X hp (X = cost of that creature)",
    effects = effects(Direct -> manipulate),
    reaction = new ManipulatorReaction)

  val Kinetician = House("Kinetician", List(

    new Creature("Magerunner", Attack(4), 12,
      "when summoned, switches places with friendly creature which has highest attack",
      effects = effects(Direct -> mage)),

    new Creature("Tricker", Attack(4), 15,
      "every turn moves to nearest unblocked slot",
      effects = effects(OnTurn -> trick)),

    Spell("Focused Force", " this turn all friendly creatures attack only target creature with attack increased by 1",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> focus)),

    Spell("Warp reality", "reduces hp of target creature to X (X = its level)",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> warp)),

    new Creature("Lone mage", Attack(7), 22,
      "every turn deals to opponent X damage (X = number of opponent`s empty slots)",
      effects = effects(OnTurn -> lone)),

    Spell("Call the fittest", "halves cost of cards of target element (each for single usage)",
      inputSpec = Some(SelectOwner(nonSpecial)),
      effects = effects(Direct -> fittest)),

    Spell("Force barrier", "doubles current and max hp of target friendly creature. It skips next turn",
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> forceBarrier)),

    manipulator),
    data = Targeting(None),
    eventListener = Some(new CustomListener(new KineticianListener)))

  Kinetician.initCards(Houses.basicCostFunc)

  def mage = { env : Env =>
    import env._
    player.slots.foldl(Option.empty[SlotUpdate]) {
      case (acc, s) if s.num == selected => acc
      case (None, s)                     => Some(s)
      case (acc @ Some(s0), s)           => if (s.get.attack > s0.get.attack) Some(s) else acc
    }.foreach { s ⇒
      player.slots.move(selected, s.num)
    }
  }

  def trick = { env : Env =>
    import env._
    nearestSlotOpposed(selected, player, opposed = false) foreach { n ⇒
      player.slots.move(selected, n)
    }
  }

  def lone = { env : Env =>
    val nbEmpty = env.otherPlayer.slots.slots.count(_.value.isEmpty)
    env.otherPlayer inflict Damage(nbEmpty, env, isAbility = true)
    env.focus()
  }

  def fittest = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    val houseIndex = slot.get.card.houseIndex
    val cards = env.player.value.desc.get.houses(houseIndex).cards.map(_.card)
    env.player.addDescMod(cards.map { c => HalveCardCost(c)} : _ *)
  }

  def forceBarrier = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    val slotState = slot.get
    slot write Some(slotState.copy(
      life = slotState.life * 2,
      maxLife = slotState.maxLife * 2,
      status = slotState.status - CardSpec.runFlag))
  }

  def warp = { env : Env =>
    val slot = env.getTargetSelectedSlot()
    val slotState = slot.get
    if (slotState.life > slotState.card.cost) {
      slot write Some(slotState.copy(life = slotState.card.cost))
    }
  }
  val focusBonus = AttackAdd(1)
  def focus = { env : Env =>
    env.player.slots foreach (_.attack add focusBonus)
    changeTarget(env)
    env.player addEffect (OnEndTurn -> oneTimePlayerEffect { env : Env =>
      env.player.slots foreach (_.attack removeFirstEq focusBonus)
    })
  }

  val attack3 = SetAttack(3)
  def manipulate = { env : Env =>
    env.otherPlayer.slots foreach (_.attack add attack3)
    env.otherPlayer addEffect (OnEndTurn -> { env : Env =>
      env.player.slots foreach (_.attack removeFirstEq attack3)
    })
  }

  class ManipulatorReaction extends Reaction {

    final def afterSubmit(slotIdOption : Option[Int]) = {
      slotIdOption foreach { slotId =>
        selected.otherPlayer.slots.slots.find(s => s.value.isDefined && s.get.id == slotId) foreach { slot =>
          val slotState = slot.get
          if (selected.slots.getOpenSlots.size > 0 || slotState.card.cost * 4 >= selected.get.life) {
            selected inflict Damage(4 * slotState.card.cost, Context(selected.playerId, Some(manipulator), selected.num), isAbility = true)
            val openSlots = selected.slots.getOpenSlots
            val targetSlot = openSlots.find(_.num == slot.num) getOrElse openSlots.head
            selected.otherPlayer.slots.move(slot.num, targetSlot.num, selected.playerId)
          }
        }
      }
    }
  }

  def getTargeting(player : PlayerUpdate) : Targeting = player.pstate.data.asInstanceOf[Targeting]
  def setTarget(player : PlayerUpdate, target : Option[Int] ) : Unit = player.updateData[Targeting](x ⇒ x.copy(target = target))

  class KineticianListener extends ChangeTargetListener {

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.submitCommand = (FuncDecorators decorate p.otherPlayer.submitCommand) after2 { (command, slotIdOption) =>
        player.slots.foreach { slot =>
          slot.get.reaction match {
            case manip : ManipulatorReaction => manip afterSubmit slotIdOption
            case _ =>
          }
        }
      }
      p.submitCommand = (FuncDecorators decorate p.submitCommand) after { command =>
        if (player.pstate.desc.descMods.nonEmpty) {
          player.removeDescMod(HalveCardCost(command.card))
        }
      }
    }
  }
}


case class HalveCardCost(card : Card) extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex == card.houseIndex) cards.map(c ⇒ if (c.card == card) c.copy(cost = c.cost / 2) else c)
    else cards
  }
}
