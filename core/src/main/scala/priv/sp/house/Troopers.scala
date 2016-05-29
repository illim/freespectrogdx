package priv.sp.house

import priv.sp._

class Trooper {
  import CardSpec._
  import GameCardEffect._

  val Trooper = House("Troopers", List(
    Spell("Conscription", "Increase troopers mana by 1", effects = effects(Direct -> addMana(1, 4))),
    new Creature("Marine", Attack(4), 17, "Deals 4 damage to summoned opponent creatures", reaction = new MarineReaction),
    new Creature("Barracks", Attack(2), 17, "Increase troopers mana growth by 1", effects = effects(OnTurn -> addMana(1, 4))),
    new Creature("Wraith", Attack(4), 24, "Every turn deals 2 damage to opponent creatures", effects = effects(OnTurn -> focus(damageCreatures(2, isAbility = true)))),
    new Creature("Goliath", Attack(6), 20, "Immune to spell and absorb 1 damage", reaction = new GoliathReaction),
    new Creature("SiegeTank", Attack(8), 29, "Every turn deals 8 damage to opponent creature with most life", effects = effects(OnTurn -> focus(siege))),
    Spell("NuclearMissile", "Deals 19 damage to opponent creatures", effects = effects(Direct -> damageCreatures(19, isSpell = true))),
    new Creature("ScienceVessel", Attack(6), 60, "When summoned deals 12 damage to opponent creatures", effects = effects(Direct -> damageCreatures(12, isAbility = true)))))

  val marine = Trooper.cards(1)
  Trooper initCards { i: Int ⇒ if (i == 0) i else i + 1 }

  private def siege = { env: Env ⇒
    import env._

    otherPlayer.slots.filleds.sortBy(_.get.life).lastOption foreach { slot ⇒
      slot inflict Damage(8, env, isAbility = true)
    }
  }

  class GoliathReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      if (d.isEffect) d.copy(amount = 0) else d.copy(amount = math.max(0, d.amount - 1))
    }
  }

  class MarineReaction extends Reaction {

    final override def onSummon(summoned: SummonEvent) {
      import summoned._
      if (selected.playerId != player.id) {
        val damage = Damage(4, Context(selected.playerId, Some(marine), selected.num), isAbility = true)
        selected.focus()
        player.slots(num) inflict damage
      }
    }
  }
}
