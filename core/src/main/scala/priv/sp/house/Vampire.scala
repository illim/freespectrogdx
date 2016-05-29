package priv.sp.house

import priv.sp._
import priv.sp.update._
import priv.util.FuncDecorators

/**
 * Introduced bullshit:
 * acolyte -> listen to enemy's damage
 */
class Vampire {
  import CardSpec._
  import GameCardEffect._


  val acolyte = new Creature("Acolyte", Attack(3), 21,
    "When enemy creature receives more than 8 damage, gives owner 1 mana of the same element.",
    reaction = new AcolyteReaction)

  val Vampire: House = House("Vampires", List(
    Spell("Dark flock", "Moves owner's creature with lowest hp in target slot and makes it invulnerable for 1 turn if its level is not higher than 9.",
      inputSpec = Some(SelectOwnerSlot),
      effects = effects(Direct -> darkFlock)),
    new Creature("Noctule", Attack(5), 17, "When deals damage to opponent, heals owner the same amount of life.", runAttack = new NoctuleAttack),
    new Creature("Ghoul", Attack(4), 20, "If opposite creature has less than 11 life, kills it at one blow.", runAttack = new GhoulAttack),
    acolyte,
    Spell("Blood ties", "destroys owner's creature and permanently increases attack of its neighbors by its attack (doesn't affect creatures with mass attack and creatures of level > 9)",
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> bloodTies)),
    new Creature("Nosferatu", Attack(5), 34, "When creature dies, heals owner by 3 and himself by 2.", reaction = new NosferatuReaction),
    new Creature("Aristocrat", Attack(7), 36, "At the beginning of turn moves in slot opposite to opponent's creature with lowest hp(can switch places with friendly creature).\nWhen kills creature deals opponent damage equal to its attack.", runAttack = new AristoAttack, effects = effects(OnTurn -> aristo)),
    new Creature("Mansion", Attack(0), 40, "When owner's non-special creature dies, replaces it with neophyte 5/14. On entering the game turns its neighbors into ghouls.", reaction = new MansionReaction, effects = effects(Direct -> ghoulify))), eventListener = Some(new CustomListener(new VampireEventListener)))

  val ghoul = Vampire.cards(2).asCreature
  val aristocrat = Vampire.cards(6).asCreature
  Vampire initCards Houses.basicCostFunc

  val neophyte = new Creature("Neophyte", Attack(5), 14, "Heals himself half of damage dealt to enemies.", runAttack = new NeophyteAttack)

  neophyte.houseIndex = Vampire.houseIndex
  neophyte.houseId = Vampire.houseId

  private def darkFlock = { env: Env ⇒
    import env._
    (player.slots reduce lowestLife) foreach { s ⇒
      val card = s.get.card
      player.slots.move(s.num, selected)
      if (card.cost < 10) {
        val slot = player.slots(selected)
        slot toggle CardSpec.invincibleFlag
        player addEffectOnce (OnTurn -> RemoveInvincible(slot.get.id))
      }
    }
  }

  private def bloodTies = { env: Env ⇒
    import env._
    val slot = getOwnerSelectedSlot()
    val attack = slot.get.attack
    slot.destroy()
    slot.filledAdjacents foreach { s ⇒
      val card = s.get.card
      if (card.cost < 9 && card.runAttack != MultiTargetAttack) {
        val bonus = AttackAdd(attack)
        s.attack add bonus
      }
    }
  }

  private def aristo = { env: Env ⇒
    import env._
    val slots = player.slots
    (otherPlayer.slots reduce lowestLife) foreach { s ⇒
      if (s.num != selected && !slots(s.num).value.exists(_.card == aristocrat)) {
        slots.move(env.selected, s.num)
      }
    }
  }

  private def ghoulify: Effect = { env: Env ⇒
    import env._
    getOwnerSelectedSlot().adjacentSlots foreach { slot ⇒
      if (slot.value.isDefined) {
        slot.destroy()
        slot add ghoul
      }
    }
  }

  class MansionReaction extends Reaction {
    final override def onDeath(dead: Dead) {
      import dead._
      if (selected.playerId == player.id && card.houseIndex < 4) {
        player.slots(num) add neophyte
      }
    }
  }

  class VampireEventListener extends HouseEventListener with AnyDeathEventListener {
    // broadcast enemy damage
    def onDamaged(card: Creature, d: Damage, slot: SlotUpdate) {
      player.slots foreach { s ⇒
        s.get.reaction match {
          case r : AcolyteReaction if r.onDamaged(player, card, d) => s.focus(blocking = false)
          case _ =>
        }
      }
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.slots.slots foreach { slot =>
        slot.onDamage = (FuncDecorators decorate slot.onDamage) after { case (slotState, d) =>
          onDamaged(slotState.card, d, slot)
        }
      }
    }
  }
}

private class NoctuleAttack extends RunAttack {
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒
        val oldl = otherPlayer.value.life
        otherPlayer inflict d
        player.heal(oldl - otherPlayer.value.life)
      case Some(_) ⇒
        slot inflict d
    }
  }
}

private class GhoulAttack extends RunAttack {
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒
        otherPlayer inflict d
      case Some(slotState) ⇒
        if (slotState.life < 11) {
          slot inflict Damage(1000, d.context)
        } else slot inflict d
    }
  }
}

class NosferatuReaction extends Reaction {
  final override def onDeath(dead: Dead) {
    selected heal 2
    selected.player heal 3
  }
}

class AcolyteReaction extends Reaction {
  def onDamaged(player : PlayerUpdate, card: Creature, d: Damage) = {
    if (d.amount > 8) {
      player.houses.incrMana(1, card.houseIndex)
      true
    } else false
  }
}

class AristoAttack extends RunAttack {
  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒
        otherPlayer inflict d
      case Some(_) ⇒
        val attack = slot.get.attack
        slot inflict d
        if (slot.value.isEmpty) {
          otherPlayer inflict Damage(attack, d.context, isAbility = true)
        }
    }
  }
}

class NeophyteAttack extends RunAttack with DamageAttack {

  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val healAmount = damageAndGet(num, d, player)
    player.slots(num) heal math.ceil(healAmount / 2f).toInt
  }
}
