package priv.sp

import CardSpec._
import priv.sp.update._

object GameCardEffect {

  class Env(val playerId: PlayerId, val updater: GameStateUpdater) extends Context {
    var selected = Context.noSelection // var to be able to reuse env?
    var card = Option.empty[Card]

    def focus() = updater.focus(selected, playerId)
    @inline def player = updater.players(playerId)
    @inline def otherPlayer = updater.players(other(playerId))
    @inline def getMana(houseIndex: Int): Int = updater.state.players(playerId).houses(houseIndex).mana
    def getOwnerSelectedSlot() = player.slots(selected)
    def getTargetSelectedSlot() = otherPlayer.slots(selected)

    //bs not safe FIXME
    def getOtherPlayerState() = updater.value.players(other(playerId))
  }

  def focus(f: Env ⇒ Unit) = { env: Env ⇒
    env.updater.focus(env.selected, env.playerId)
    f(env)
  }
  def damage(amount: Int, isAbility: Boolean = false, isSpell: Boolean = false) = { env: Env ⇒
    val d = Damage(amount, env, isAbility, isSpell)
    env.otherPlayer inflict d
  }
  def damageCreatures(amount: Int, isAbility: Boolean = false, isSpell: Boolean = false): Effect = { env: Env ⇒
    val d = Damage(amount, env, isAbility, isSpell)
    env.otherPlayer.slots inflictCreatures d
  }
  def damageCreature(amount: Int, isAbility: Boolean = false, isSpell: Boolean = false): Effect = { env: Env ⇒
    val d = Damage(amount, env, isAbility, isSpell)
    env.otherPlayer.slots(env.selected) inflict d
  }
  def massDamage(amount: Int, isAbility: Boolean = false, isSpell: Boolean = false, immuneSelf: Boolean = false) = { env: Env ⇒
    val d = Damage(amount, env, isAbility = isAbility, isSpell = isSpell)
    env.otherPlayer.slots inflictCreatures d
    if (immuneSelf) {
      env.player.slots.value foreach {
        case (num, _) ⇒
          if (num != env.selected) env.player.slots(num) inflict d
      }
    } else {
      env.player.slots inflictCreatures d
    }
  }

  def heal(amount: Int) = { env: Env ⇒ env.player.heal(amount) }
  def healCreature(amount: Int): Effect = { env: Env ⇒
    env.player.slots(env.selected) heal amount
  }
  def healCreatures(amount: Int): Effect = { env: Env ⇒
    env.player.slots healCreatures amount
  }

  def addMana(amount: Int, houseIndex: Int*) = { env: Env ⇒
    env.player.houses.incrMana(amount, houseIndex: _*)
  }
  def addDescMod(mod: DescMod) = { env: Env ⇒ env.player.addDescMod(mod) }

  def fillEmptySlots(creature : Creature) = { env : Env =>
    def spawnCreature(num: Int) : Unit = {
      val slot = env.player.slots(num)
      if (slot.value.isEmpty) {
        slot add creature
      }
    }
    env.player.value.slotList foreach spawnCreature
  }

  case class oneTimePlayerEffect(f : Env => Unit) extends Function[Env, Unit] {
    def apply(env: Env) {
      f(env)
      env.player removeEffect (_ == this)
    }
  }
}

case class Ability(card: Card, ability: Card) extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex == 4) {
      cards.map { c ⇒
        if (c.card == card) {
          CardDesc(ability)
        } else c
      }
    } else cards
  }
}
