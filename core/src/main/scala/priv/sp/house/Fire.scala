package priv.sp.house

import priv.sp._
import priv.sp.update._

trait Fire {
  import CardSpec._
  import GameCardEffect._

  val Fire = House("fire", List(
    new Creature("Goblin", Attack(4), 16, "Every turn deals 2 damage to owner adjacent cards",
      effects = effects(OnTurn -> goblinBerserker)),
    new Creature("Wall of flame", Attack(0), 5, "Deals 5 damage to opponent creatures when summoned",
      effects = effects(Direct -> damageCreatures(5, isAbility = true))),
    new Creature("Fire monk", Attack(3), 13, "Every turn increase fire mana growth by 1",
      effects = effects(OnTurn -> addMana(1, 0))),
    new Creature("Drake", Attack(4), 18, "Attack the turn he is summoned",
      status = runFlag),
    new Creature("Orc chieftain", Attack(3), 16, "Increase attack of adjacent card by 2",
      reaction = new OrcSlotReaction),
    Spell("Flame wave", "Deals 9 damage to opponent creatures",
      effects = effects(Direct -> damageCreatures(9, isSpell = true))),
    new Creature("Bull Commander", Attack(6), 20, "Increase attack of owner card by 1",
      reaction = new BullSlotReaction),
    new Creature("Blargl", Attack(8), 26, "Deals 4 damage to every creature when summoned",
      effects = effects(Direct -> massDamage(4, isAbility = true, immuneSelf = true))),
    Spell("Inferno", "Deals 18 damage to target and 10 to other opponent creatures",
      inputSpec = Some(SelectTargetCreature), effects = effects(Direct -> inferno)),
    new Creature("Fire Elemental", AttackSources().add(ManaAttack(0)), 36,
      "Fire Elemental deals 3 damage to opponent creatures when summoned",
      effects = effects(
        Direct -> damageCreatures(3, isAbility = true),
        Direct -> focus(damage(3, isAbility = true)),
        OnTurn -> addMana(1, 0))),
    Spell("Apocalypse",
      (state : GameState, playerId : PlayerId) =>
        "Damage any creature and opponent by 8 + fire mana ["+(8 + state.players(playerId).houses(0).mana)+"]",
      effects = effects(Direct -> armageddon)),
    new Creature("Dragon", Attack(9), 41, "Increase spell damage by 50%",
      mod = Some(new SpellMod(x ⇒ math.ceil(x * 1.5).intValue)))), houseIndex = 0)

  private def goblinBerserker = { env: Env ⇒
    val damage = Damage(2, env, isAbility = true)
    val targets = env.player.slots(env.selected).adjacentSlots filter (_.value.isDefined)
    if (targets.nonEmpty) {
      env.focus()
      targets foreach (_.inflict(damage))
    }
  }

  private def inferno = { env: Env ⇒
    import env._

    val damage = Damage(10, env, isSpell = true)
    otherPlayer.slots foreach { slot ⇒
      slot.inflict(
        if (slot.num == selected) Damage(18, env, isSpell = true) else damage)
    }
  }

  private def armageddon = { env: Env ⇒
    import env._

    val d = Damage(getMana(0) + 8, env, isSpell = true)
    env.otherPlayer inflict d
    massDamage(d.amount, isSpell = true)(env)
  }
}

case class OrcAttackBonus(orcPos: Int) extends AttackFunc with UniqueAttack { def apply(attack: Int) = attack + 2 }
case class BullAttackBonus(bullPos: Int) extends AttackFunc with UniqueAttack { def apply(attack: Int): Int = attack + 1 }

private class OrcSlotReaction extends AttackBonusReaction {
  final def cond(selected: Int, num: Int) = math.abs(selected - num) == 1
  def getBonus(selected: Int) = OrcAttackBonus(selected)
}

class BullSlotReaction extends AttackBonusReaction {
  final def cond(selected: Int, num: Int) = selected != num
  def getBonus(selected: Int) = BullAttackBonus(selected)
}

