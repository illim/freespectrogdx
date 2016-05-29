package priv.sp.house

import priv.sp._

trait Air {
  import CardSpec._
  import GameCardEffect._

  val Air = House("air", List(
    new Creature("Faerie novice", Attack(4), 11, "Increase spell damage by 1", mod = Some(new SpellMod(x ⇒ x + 1))),
    new Creature("Griffin", Attack(3), 15, "If Air mana >= 5 deals 5 damage to opponent when summoned",
      effects = effects(Direct -> { env: Env ⇒
        if (env.getMana(2) > 4) env.otherPlayer inflict Damage(5, env)
      })),
    Spell("Call to thunder", "Deals 6 damage to target creature and opponent",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> damageCreature(6, isSpell = true), Direct -> damage(6, isSpell = true))),
    new Creature("Air medic", Attack(4), 19, "When summoned heals owner by amount of earth mana(limited to 10)",
      effects = effects(Direct -> { env: Env ⇒
        env.focus()
        env.player heal math.min(env.getMana(3), 10)
      })),
    new Creature("Lightning fence", Attack(0), 28, "Every turn deals 4 damage to opponent", effects = effects(OnTurn -> focus(damage(4, isAbility = true)))),
    Spell("Lightnin",
      (state : GameState, playerId : PlayerId) =>
        "Deals (5 + air power)["+(5+state.players(playerId).houses(2).mana)+"] damage to opponent",
      effects = effects(Direct -> { env: Env ⇒ env.otherPlayer inflict Damage(5 + env.getMana(2), env, isSpell = true) })),
    new Creature("Phoenix", Attack(6), 18, "Can reborn if fire mana >= 10", reaction = new PhoenixReaction),
    Spell("Chain lightning", "Deals 9 damage to opponent and his creatures", effects = effects(Direct -> damageCreatures(9, isSpell = true), Direct -> damage(9, isSpell = true))),
    new Creature("Cloud", Attack(4), 20, "Attack all opponent creatures", runAttack = MultiTargetAttack),
    Spell("Twister", "destroy target",
      inputSpec = Some(SelectTargetCreature),
      effects = effects(Direct -> { env: Env ⇒ env.otherPlayer.slots(env.selected).overridableDestroy() })),
    new Creature("Air Elemental", AttackSources().add(ManaAttack(2)), 44, "Air elemental deals 8 damage to opponent when summoned", effects = effects(Direct -> focus(damage(8, isAbility = true)), OnTurn -> addMana(1, 2))),
    new Creature("Titan", Attack(9), 40, "Deals 15 damage to opposite creature when summoned",
      effects = effects(Direct -> { env: Env ⇒
        env.focus()
        env.otherPlayer.slots(env.selected) inflict Damage(15, env, isAbility = true)
      }))), houseIndex = 2)

}

class PhoenixReaction extends Reaction {
  final override def onMyDeath(dead: Dead) {
    import dead._
    if (!dead.isDestroy && player.houses.value(0).mana > 9) {
      player.slots(num) add card
    }
  }
}
