package priv.sp.house

import priv.sp._
import priv.sp.update._

/**
 * Introduced bullshit:
 * fortune -> in damageSlot, reeval slot after reaction because fortune update her data
 *
 * Localized bs:
 * chain controler -> effect applied in slot order so it's bugged when a move is involved
 */
class JunkMage {
  import CardSpec._
  import GameCardEffect._

  private val trash = new Creature("junk.trash", Attack(2), 11)
  private val trashCyborg = new Creature("junk.cyborg", Attack(4), 27, I18n("junk.cyborg.description"),
    effects = effects(Direct -> spawnTrash, OnTurn -> gatherTrash))
  private val jf = new Creature("junk.fortune", Attack(3), 15, I18n("junk.fortune.description"),
    reaction = new JFReaction, effects = effects(OnEndTurn -> resetProtect), data = java.lang.Boolean.FALSE)

  val Junk: House = House("junk", List(
    new Creature("junk.screamer", AttackSources(Some(2), Vector(ScreamerAttackSource)), 12, I18n("junk.screamer.description"), reaction = new ScreamerReaction),
    Spell("junk.poison", I18n("junk.poison.description"),
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> poisonFlower)),
    jf,
    new Creature("junk.chain", Attack(4), 18, I18n("junk.chain.description"),
      reaction = new ChainControllerReaction),
    new Creature("junk.assassin", Attack(6), 27, I18n("junk.assassin.description"),
      effects = effects(OnEndTurn -> roam)),
    new Creature("junk.factory", Attack(4), 29, I18n("junk.factory.description"), reaction = new FactoryReaction),
    new Creature("junk.bot", Attack(8), 29, I18n("junk.bot.description"), reaction = new RecyclingBotReaction),
    trashCyborg), eventListener = Some(new CustomListener(new JunkEventListener)))

  trash.cost = 1
  Junk.initCards(Houses.basicCostFunc)
  Junk.addAdditionalCards(trash)

  private val screamer = Junk.cards(0)

  private def resetProtect = { env: Env ⇒
    env.player.slots(env.selected).setData(Boolean.box(false))
  }

  private def spawnTrash = GameCardEffect fillEmptySlots trash

  private def gatherTrash: CardSpec.Effect = { env: Env ⇒
    val slots = env.player.slots
    // bugged with card moves
    val trashStates = (List.empty[SlotState] /: baseSlotRange) { (acc, i) ⇒
      val slot = slots(i)
      if (acc.size < 2 && slot.value.isDefined && slot.get.card == trash) {
        val state = slot.get
        slot.destroy()
        state :: acc
      } else acc
    }
    if (trashStates.nonEmpty) {
      val life = trashStates.map(_.life).sum
      val attack = trashStates.size * trash.attack.base.get
      // get first !
      slots.slots find (x ⇒ x.value.isDefined && x.get.card == trashCyborg) foreach { slot ⇒
        env.updater.focus(slot.num, env.playerId)
        val s = slot.get
        slot.write(Some(s.copy(life = s.life + life)))
        slot.attack.add(AttackAdd(attack))
      }
    }
  }

  private def roam = { env: Env ⇒
    import env._
    val otherSlots = otherPlayer.slots
    if (otherSlots(selected).value.isEmpty) {
      nearestSlotOpposed(selected, player).foreach { n ⇒
        val slots = player.slots
        val dest = slots(n)
        otherSlots(n) inflict Damage(5, env, isAbility = true)
        slots.move(selected, n)
      }
    }
  }

  private def poisonFlower = { env: Env ⇒
    import env._

    val damage = Damage(5, env, isSpell = true)
    val slot = getOwnerSelectedSlot
    val h = slot.get.card.houseIndex
    slot.inflict(damage)
    player.houses.incrMana(-1, h)
    if (slot.oppositeSlot.value.isDefined) {
      val hopp = slot.oppositeSlot.get.card.houseIndex
      slotInterval(selected - 1, selected + 1) flatMap { num ⇒
        val oppSlot = otherPlayer.slots(num)
        oppSlot.value map { slot ⇒
          oppSlot inflict damage
        }
      }
      otherPlayer.houses.incrMana(-1, hopp)
    }
  }

  private class ScreamerReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) = {
		if (slot.get.card == screamer) {
			setScreamerDirty(slot.slots)
			
			val slotState = slot.get
			slot write Some(slotState.copy(life = slotState.life + 2))
		}
    }
	

	
    final override def onMyDeath(dead: Dead) = setScreamerDirty(dead.player.slots)
    def setScreamerDirty(slots: SlotsUpdate) {
      slots.foreach { s ⇒
        if (s.get.card == screamer) {
          s.attack.setDirty()
        }
      }
    }
  }

  case object ScreamerAttackSource extends AttackStateFunc {
    def apply(attack: Int, player: PlayerUpdate) = {
      val nbScreamers = player.slots.slots.count { s ⇒
        s.value.isDefined && s.get.card == screamer
      }
      attack + nbScreamers
    }
  }

  class JunkEventListener extends HouseEventListener with OwnerDeathEventListener {
    def protect(slot: SlotUpdate, damage: Damage) = {
      player.slots.foldl(damage) { (acc, s) ⇒
        val sc = s.get.card
        if (sc == jf) {
          s.get.reaction.onProtect(DamageEvent(acc, Some(slot.num), player))
        } else acc
      }
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.slots.slots foreach { slot ⇒
        slot.protect.modifyResult(d ⇒ protect(slot, d))
      }
    }
  }
}

class JFReaction extends Reaction {
  final override def onProtect(d: DamageEvent) = {
    import d._
    if (!selected.get.data.asInstanceOf[Boolean]
      && d.target.isEmpty
      && d.damage.amount > 0) {
      player.updater.focus(selected.num, player.id, blocking = false)
      selected setData java.lang.Boolean.TRUE
      d.damage.copy(amount = math.max(0, d.damage.amount - 6))
    } else d.damage
  }
}

trait MirrorSummon extends Reaction {
  def maxCost: Int
  var healOnSummon = 0
  final override def onSummon(summoned: SummonEvent) {
    import summoned._
    val step = selected.num - num
    if (selected.playerId == player.id
      && math.abs(step) == 1
      && card.cost < maxCost + 1) {
      val pos = selected.num + step
      if (player.value.isInSlotRange(pos)) {
        val slot = player.slots(pos)
        if (slot.value.isEmpty) {
          selected.focus()
          slot.add(card)
        }
        if (healOnSummon != 0) {
          selected heal healOnSummon
        }
      }
    }
  }
}
class ChainControllerReaction extends MirrorSummon {
  val maxCost = 3
  final override def onDeath(dead: Dead) {
    import dead._
    val step = num - selected.num
    if (card.cost < 6 && math.abs(step) == 1) {
      def getAt(n: Int) = {
        if (player.value.isInSlotRange(n)) {
          player.slots(n).value match {
            case Some(slot) if slot.card.cost < 6 ⇒ Some(n)
            case _                                ⇒ None
          }
        } else None
      }

      (getAt(num + step) orElse getAt(selected.num - step)).foreach { dest ⇒
        selected.focus()
        player.slots.move(dest, num)
      }
    }
  }
}

class FactoryReaction extends MirrorSummon {
  val maxCost = 5
  healOnSummon = 5
}

class RecyclingBotReaction extends Reaction {
  final override def onDeath(dead: Dead) {
    import dead._
    selected.value foreach { botSlot ⇒
      selected.focus()
      player heal 2
      selected heal 10
    }
  }
}
