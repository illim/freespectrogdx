package priv.sp

import collection._
import java.io.Serializable

object GameState {

  def toString(s: GameState) = {
    s.players.map { p ⇒
      "houses " + p.houses.map(_.mana).mkString(",") + ", life " + p.life +
        ", slots " + p.slotList.map { i ⇒
          p.slots.get(i).map { s ⇒
            s.card.name + "/" + s.life
          }.getOrElse(".")
        }.mkString(",") + "\n"
    }.mkString
  }
}
case class GameState(players: List[PlayerState])
case class PlayerState(
    houses: PlayerState.HousesType,
    desc: DescReader,
    slots: PlayerState.SlotsType = PlayerState.emptySlots,
    slotList: List[Int] = baseSlotList,
    life: Int = 60,
    effects: List[CardSpec.PhaseEffect] = Nil,
    data: AnyRef = null,
    transitions: List[Transition] = Nil) { // not great using this field to pass parameter

  def isDisabled = desc.get.houses forall { h ⇒
    val hs = houses(h.house.houseIndex)
    h.cards forall (c ⇒ !c.isAvailable(hs))
  }

  def isInSlotRange(n: Int) = slotList.contains(n)
}
class HouseState(val mana: Int) extends AnyVal with Serializable

object SlotState {
  @inline def addLife(slot: SlotState, amount: Int) = {
    slot.copy(life = math.min(slot.card.life, slot.life + amount))
  }
  val currentId = new java.util.concurrent.atomic.AtomicInteger
}
case class SlotState(card: Creature, life: Int, maxLife : Int, status: Int, attackSources: AttackSources, attack: Int, target: List[Int], id: Int, reaction: Reaction, data: AnyRef = null) {

  def inflict(damage: Damage): Option[SlotState] = {
    if (has(CardSpec.invincibleFlag)) Some(this)
    else {
      val newlife = card.inflict(damage, life)
      if (newlife < 1) None else Some(copy(life = newlife))
    }
  }

  def has(flag: Int) = (status & flag) != 0
  def isRunnable = has(CardSpec.runFlag) & !has(CardSpec.onHold)
}

// Description (should not change during the game)
case class GameDesc(players: Vector[PlayerDesc])
case class PlayerDesc(houses: Vector[PlayerHouseDesc]) {

  def getIndexOfCardInHouse(card: Card) = {
    houses.find(_.cards.contains(card)).map(_.cards.indexOf(card)).getOrElse(-1)
  }
}
case class PlayerHouseDesc(house: House, cards: Vector[CardDesc])
case class CardDesc(card: Card, cost: Int, enabled: Boolean) {
  def isAvailable(house: HouseState) = cost <= house.mana && enabled
}

object PlayerState {
  type SlotsType = immutable.TreeMap[Int, SlotState]
  type HousesType = Vector[HouseState]
  val emptySlots = immutable.TreeMap.empty[Int, SlotState]
  def init(houseState: PlayerState.HousesType, desc: PlayerDesc) = {
    val special = desc.houses(4).house
    PlayerState(houseState, new DescReader(desc), effects = special.effects, data = special.data)
  }
  def openSlots(p: PlayerState): List[Int] = {
    p.slotList.filter { num ⇒
      p.slots.get(num) match {
        case Some(s) if !s.card.isAltar ⇒ false
        case _                          ⇒ true
      }
    }
  }
}

object CardDesc {
  def apply(c: Card): CardDesc = CardDesc(c, c.cost, true)
}

case class DescReader(init: PlayerDesc, descMods: Vector[DescMod] = Vector.empty) {
  val get = if (descMods.isEmpty) init else {
    PlayerDesc(init.houses.map { h ⇒
      PlayerHouseDesc(h.house, modify(h.house, h.cards))
    })
  }

  def modify(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
    (cards /: descMods) { (acc, mod) ⇒
      mod(house, acc)
    }
  }

  def add(mods: DescMod*) = copy(descMods = descMods ++ mods)
  def insert(mod: DescMod) = copy(descMods = mod +: descMods)
  def remove(mod: DescMod) = {
    val idx = descMods.indexOf(mod)
    if (idx != -1) {
      copy(descMods = descMods.patch(idx, Vector.empty, 1))
    } else this
  }
}

// crappy hard coded transitions
sealed trait Transition {
  def name: String
  def playerId: PlayerId
}
case class WaitPlayer(playerId: PlayerId, name: String = null) extends Transition
