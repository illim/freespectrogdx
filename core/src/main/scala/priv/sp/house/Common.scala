
package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update._
import priv.util.FuncDecorators

case object OneAttackBonus extends AttackFunc { def apply(attack: Int) = attack + 1 }
case class AttackAdd(bonus: Int) extends AttackFunc { def apply(attack: Int) = attack + bonus }
case class SetAttack(x: Int) extends AttackFunc { def apply(attack: Int) = x }

class RemoveAttack(attack: AttackSource) extends Function[Env, Unit] {
  def apply(env: Env) {
    env.player.slots foreach (_.attack.removeFirstEq(attack))
  }
}

case class RemoveInvincible(slotId: Int) extends Function[Env, Unit] {
  def apply(env: Env) {
    env.player.slots.slots.find(s ⇒ s.value.isDefined && s.get.id == slotId) foreach { s ⇒
      s.toggleOff(CardSpec.invincibleFlag)
    }
  }
}

// hack for warp
trait UniqueAttack

//crap
case class UnMod(mod: DescMod) extends Function[Env, Unit] {
  def apply(env: Env) {
    env.player removeDescMod mod
  }
}

case class LowerCostMod(indexes : Set[Int]) extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (indexes.contains(house.houseIndex)) cards.map(c ⇒ c.copy(cost = math.max(0, c.cost - 1)))
    else cards
  }
}

case object HideBasicMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex == 4) cards
    else cards.map(_.copy(enabled = false))
  }
}

case object HideSpecialMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex != 4) cards
    else cards.map(_.copy(enabled = false))
  }
}

case object HideSpellMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards.map { c ⇒
      if (c.card.isSpell) c.copy(enabled = false)
      else c
    }
  }
}

object HideCreatureMod extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards.map { c ⇒
      if (!c.card.isSpell) c.copy(enabled = false)
      else c
    }
  }
}

case class AttackFactor(fact: Float) extends AttackFunc {
  def apply(attack: Int): Int = math.ceil(attack * fact).toInt
}

case object SkipTurn extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards.map(c ⇒ c.copy(enabled = false))
  }
}

abstract class AttackBonusReaction extends Reaction {
  def cond(selected: Int, num: Int): Boolean
  def getBonus(selected: Int): AttackSource

  final override def onAdd(slot: SlotUpdate) = {
    val bonus = getBonus(selected.num)
    if (selected.num == slot.num) {
      slot.slots foreach { s ⇒
        if (cond(s.num, slot.num)) s.attack add bonus
      }
    } else if (cond(selected.num, slot.num)) {
      slot.attack add bonus
    }
  }

  final override def onRemove(slot: SlotUpdate): Unit = {
    if (selected.num != slot.num && cond(selected.num, slot.num)) {
      slot.attack removeFirst getBonus(selected.num)
    }
  }

  final override def onMyRemove(dead: Option[Dead]) = {
    val bonus = getBonus(selected.num)
    selected.slots foreach { slot ⇒
      if (cond(slot.num, selected.num)) {
        slot.attack removeFirst bonus
      }
    }
  }
}

trait OwnerDeathEventListener extends HouseEventListener {
  def reactDead(dead: Dead) {
    if (dead.player.id == player.id) {
      player.slots foreach { s ⇒
        if (s.num != dead.num) {
          val card = s.get.card
          if (card.isSpecial) {
            s.get.reaction onDeath dead
          }
        }
      }
    }
  }
  override def init(p: PlayerUpdate) {
    super.init(p)
    p.slots.onDead = (FuncDecorators decorate p.slots.onDead) after { dead ⇒ reactDead(dead) }
  }
}

trait AnyDeathEventListener extends HouseEventListener {
  def reactDead(dead: Dead) {
    player.slots foreach { s ⇒
      if (dead.player.id != player.id || s.num != dead.num) {
        val card = s.get.card
        if (card.isSpecial) {
          s.get.reaction onDeath dead
        }
      }
    }
  }
  override def init(p: PlayerUpdate) {
    super.init(p)
    p.slots.onDead = (FuncDecorators decorate p.slots.onDead) after reactDead
    p.otherPlayer.slots.onDead = (FuncDecorators decorate p.otherPlayer.slots.onDead) after reactDead
  }
}

trait OppDeathEventListener extends HouseEventListener {
  def reactDead(dead: Dead) {
    player.slots foreach { s ⇒
      val card = s.get.card
      if (card.isSpecial) {
        s.get.reaction onDeath dead
      }
    }
  }
  override def init(p: PlayerUpdate) {
    super.init(p)
    p.otherPlayer.slots.onDead = (FuncDecorators decorate p.otherPlayer.slots.onDead) after reactDead
  }
}

trait DamageAttack {

  // return amount of damage done
  def damageAndGet(num: Int, d: Damage, player: PlayerUpdate) = {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒
        val oldl = otherPlayer.value.life
        otherPlayer inflict d
        oldl - otherPlayer.value.life
      case Some(slotState) ⇒
        val oldl = slotState.life
        slot.inflict(d)
        val newl = slot.value.map(_.life) getOrElse 0
        oldl - newl
    }
  }

  def damageCreatureAndGet(num: Int, d: Damage, player: PlayerUpdate) = {
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    slot.value match {
      case None ⇒ 0
      case Some(slotState) ⇒
        val oldl = slotState.life
        slot.inflict(d)
        val newl = slot.value.map(_.life) getOrElse 0
        oldl - newl
    }
  }
}

object PlayerTask {
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}
class CountDown(val count: Int, f: Env ⇒ Unit, val id: Int = PlayerTask.currentId.incrementAndGet) extends Function[Env, Unit] {
  def apply(env: Env) {
    val c = count - 1
    if (c == 0) {
      f(env)
      env.player removeEffect (_ == this)
    } else {
      env.player mapEffect { e ⇒
        if (e == this) {
          new CountDown(count - 1, f, id)
        } else e
      }
    }
  }
  override def equals(o: Any) = {
    o match {
      case c: CountDown ⇒ c.id == id
      case _            ⇒ false
    }
  }
  override def hashCode() = id
}

trait ReactionWithData[A <: AnyRef] extends Reaction {
  def updateData(f: A ⇒ A) {
    selected write selected.value.map(x ⇒ x.copy(data = f(x.data.asInstanceOf[A])))
  }
  def getData = selected.get.data.asInstanceOf[A]
}

case class Destroyed(card: Card) extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    if (house.houseIndex != card.houseIndex) cards
    else cards filter { c ⇒ c.card != card }
  }
}

case class Destroyeds(excls: Set[Card]) extends DescMod {
  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    cards filterNot { c ⇒ excls contains c.card }
  }
}


case class Targeting(target: Option[Int] = None)

trait ChangeTarget {

  def getTargeting(player : PlayerUpdate) : Targeting

  def setTarget(player : PlayerUpdate, target : Option[Int] ) : Unit

  val changeTarget = { env: Env ⇒
    import env._
    val target = otherPlayer.slots(selected).get
    setTarget(player, Some(target.id))
    player.slots foreach { s ⇒
      s.write(s.value.map(_.copy(target = List(selected))))
    }
    player addEffectOnce (OnEndTurn -> new RecoverTarget)
  }

  class RecoverTarget extends Function[Env, Unit] {
    def apply(env: Env) {
      if (getTargeting(env.player).target.isDefined) {
        recoverTarget(env.player)
      }
    }
  }

  def recoverTarget(player: PlayerUpdate) {
    player.slots foreach { s ⇒
      s write s.value.map(_.copy(target = List(s.num)))
    }
    setTarget(player, None)
  }

  trait ChangeTargetListener extends HouseEventListener{

    def reactDead(dead: Dead) {
      getTargeting(player).target foreach { id ⇒
        if (id == dead.slot.id) {
          recoverTarget(player)
        }
      }
    }
    override def init(p: PlayerUpdate) {
      super.init(p)
      p.otherPlayer.slots.onDead = (FuncDecorators decorate p.otherPlayer.slots.onDead) after reactDead
    }
  }

}
