package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update._

/**
 * Introduced bullshit:
 * stranger ->
 *        proxy for houseeventlistener
 */
// FIXME: schizo when unbridle
object Warp {
  val photographer = new Creature("Photographer", Attack(4), 16,
    "If adjacent creature die, he's replaced by photographer with same life.",
    reaction = new PhotoReaction)

  val Warp = House("Warp", List(

    new Creature("Errant", Attack(4), 19, "Hide in shadow after killing a creature, come back when damaged.",
      runAttack = new ErrantAttack,
      reaction = new ErrantReaction),

    Spell("EarthQuake", "Deals to opponent creatures damage equals to difference between owner and opponent mana",
      effects = effects(Direct -> quake)),

    new Creature("Cloak", Attack(2).add(new CloakAttack), 15,
      "When die restore the creature.\n" +
        "Attack is added to underlying creature attack",
      inputSpec = Some(SelectOwnerCreature),
      reaction = new CloakReaction),

    photographer,

    new Creature("Schizo", Attack(5), 22,
      "When summoned, opposite creature lose his abilities until schizo die.",
      reaction = new SchizoReaction),

    new Creature("Ram", Attack(6), 26,
      "Opposite creature is destroyed and opponent get his mana back -2.\n" +
        "(Cost of the creature -2)",
      effects = effects(Direct -> ram)),

    new Creature("Stranger", AttackSources().add(new StrangerAttack), 30,
      "Attack is highest opponent mana.\n" +
        "When summoned, take effects of opposite slot.(at least try to!)\n" +
        " -immediate effects are not applied\n" +
        " -can't duplicate effect to attack multiple targets", effects = effects(Direct -> merge)),

    new Creature("Warp Queen", Attack(6), 32,
      "Opponent creatures lose their ability until end of next owner turn.\n" +
      "Deals 4 damage to each of them", effects = effects(Direct -> warp))),

    eventListener = Some(OpponentListener({
      case _ : Limbo.LimboEventListener
         | _ : Colors.ColorListener
         | _ : Soulbinder.SoulListener
         | _ : SoulReaper.SoulReaperListener => new WarpEventListener {}
      case inner => new ProxyEventListener(inner) with WarpEventListener
    })))

  val stranger = Warp.cards(6)
  Warp initCards Houses.basicCostFunc

  def quake = { env: Env ⇒
    import env._
    val houses = player.value.houses
    otherPlayer.slots foreach { slot ⇒
      val hidx = slot.get.card.houseIndex
      val d = Damage(
        math.abs(houses(hidx).mana - otherPlayer.getHouses(slot.get.card.houseIndex).mana),
        env, isSpell = true)
      slot inflict d
    }
  }

  def merge = { env: Env ⇒
    import env._
    val oppSlot = otherPlayer.slots(selected)
    oppSlot.value.foreach { opp ⇒
      val slot = player.slots(selected)
      val s = slot.get
      slot.destroy()
      val merged = new MergeStranger(s.card, opp.card)
      slot add SlotState(merged, s.life, s.maxLife, s.status, s.card.attack, player.slots.getAttack(slot, s.card.attack), s.target, s.id, merged.newReaction, merged.data)
    }
  }
  def ram = { env: Env ⇒
    import env._
    val oppSlot = otherPlayer.slots(selected)
    oppSlot.value foreach { s ⇒
      oppSlot.destroy()
      otherPlayer.houses.incrMana(math.max(0, s.card.cost - 2), s.card.houseIndex)
    }
  }
  def warp = { env: Env ⇒
    import env._
    otherPlayer.slots foreach { slot ⇒
      bridle(slot remove None, slot)
    }
    otherPlayer.slots inflictCreatures Damage(4, env, isAbility = true)
    player addEffect (OnEndTurn -> new CountDown(2, { env: Env ⇒
      env.otherPlayer.slots foreach unbridle
    }))
  }

  private val cache = collection.mutable.Map.empty[Card, MereMortal]

  def bridle(s: SlotState, slot: SlotUpdate) {
    val c = if (s.card.isInstanceOf[MergeStranger]) new MereMortal(s.card) else cache.getOrElseUpdate(s.card, new MereMortal(s.card)) // hack
    val reaction = c.newReaction
    reaction use slot
    slot write Some(SlotState(c, s.life, s.maxLife, s.status, s.attackSources, slot.slots.getAttack(slot, s.attackSources), s.target, s.id, reaction, s.data))
  }
  def unbridle(slot: SlotUpdate) {
    slot.value foreach { s ⇒
      s.card match {
        case m: MereMortal ⇒
          val removed = slot remove None
          val reaction = m.c.newReaction
          reaction use slot

          slot add SlotState(
              m.c, removed.life, removed.maxLife, removed.status,
              removed.attackSources, slot.slots.getAttack(slot, removed.attackSources),
              removed.target, removed.id, reaction, removed.data)
        case _ ⇒
      }
    }
  }

  class PhotoReaction extends Reaction {
    final override def onDeath(dead: Dead) {
      if (dead.card != photographer
        && dead.player.id == selected.playerId
        && Math.abs(dead.num - selected.num) == 1) {
        selected.value.foreach { value =>
          val slot = dead.player.slots(dead.num)
          slot add photographer
          slot.write(Some(slot.get.copy(life = value.life)))
        }
      }
    }
  }

  class SchizoReaction extends Reaction {

    override def onAdd(slot: SlotUpdate) {
      if (selected.num == slot.num) {
        val oppSlot = selected.oppositeSlot
        oppSlot.value foreach { s ⇒
          oppSlot remove None
          selected setData new Integer(s.id)
          bridle(s, oppSlot)
        }
      }
    }
    override def onMyRemove(dead: Option[Dead]) {
      val s = selected.get
      if (s.data != null) {
        selected.otherPlayer.slots.slots find { t ⇒ // in case he has moved !
          t.value exists { y ⇒
            if (y.id == s.data.asInstanceOf[Integer].intValue) {
              unbridle(t)
              true
            } else false
          }
        }
      }
      selected.get
    }
  }

  // code horror
  trait WarpEventListener extends OwnerDeathEventListener {
    private def isStranger(card: Card) = {
      card == stranger || card.isInstanceOf[MergeStranger]
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      if (p.otherPlayer.value.data != null && !p.otherPlayer.value.data.isInstanceOf[SoulReaperData]) { // HACK
        player setData p.otherPlayer.value.data
      }
      p.otherPlayer.houses.update after { _ ⇒ refreshStranger() }
    }

    def refreshStranger() {
      if (player.getSlots.values.exists(s ⇒ isStranger(s.card))) {
        player.slots.filleds.withFilter(s ⇒ isStranger(s.get.card)) foreach { s ⇒
          s.attack.setDirty()
        }
      }
    }
  }
}

class ErrantAttack extends RunAttack {

  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    if (SingleTargetAttack.attack(target, d, player)) {
      player.slots(d.context.selected) toggle CardSpec.pausedFlag
    }
  }
}

class ErrantReaction extends Reaction {
  override def onMyDamage(damage: Damage) {
    selected.value foreach { s ⇒
      if (s has pausedFlag) {
        selected toggleOff pausedFlag
      }
    }
  }
}

class StrangerAttack extends AttackStateFunc {
  def apply(attack: Int, player: PlayerUpdate): Int = {
    attack + player.otherPlayer.getHouses.maxBy(_.mana).mana
  }
}

class CloakReaction extends Reaction {
  override def onSpawnOver = {
    val s = selected.get
    selected remove None
    Some(new CloakSlotMod(s))
  }

  override def onAdd(slot: SlotUpdate) = {
    selected.attack.setDirty()
  }

  override def onMyDeath(dead: Dead) {
    import dead._
    val cloaked = dead.slot.data.asInstanceOf[SlotState]
    if (cloaked != null) {
      val slot = player.slots(num)
      val card = cloaked.card
      slot add SlotState(card, cloaked.life, cloaked.maxLife, cloaked.status, card.attack, player.slots.getAttack(slot, card.attack), List(slot.num), cloaked.id, cloaked.reaction, card.data)
    }
  }
}

class CloakSlotMod(cloaked: SlotState) extends SlotMod {
  def apply(slotState: SlotState) = {
    slotState.copy(data = cloaked)
  }
}

class CloakAttack extends AttackSlotStateFunc {
  def apply(attack: Int, slot: SlotUpdate) = {
    slot.value.map { state =>
      state.data match {
        case s : SlotState => attack + s.attack
        case _ => attack
      }
    } getOrElse attack
  }
}

class MereMortalReaction(c: Creature) extends Reaction {
  val innerReaction = c.newReaction
  override def use(s: SlotUpdate) { super.use(s); innerReaction.use(s) }
  final override def cleanUp() {
    innerReaction.cleanUp()
  }
}

class MereMortal(val c: Creature)
    extends Creature(c.name, c.attack, c.life, status = c.status, reaction = new MereMortalReaction(c), data = c.data) {
  houseId = c.houseId
  houseIndex = c.houseIndex
  cost = c.cost
  id = c.id
}

class MergeStranger(s: Creature, c: Creature)
    extends Creature(
      s.name,
      s.attack,
      s.life,
      c.description,
      s.inputSpec, // don't care
      c.effects,
      c.mod,
      c.newReaction,
      c.data,
      if (c.runAttack.isMultiTarget) s.runAttack else c.runAttack,
      c.isAltar,
      c.status) {
  houseId = s.houseId
  houseIndex = s.houseIndex
  cost = s.cost
  id = c.id // !!!!
}
