package priv.sp.house

import priv.sp._
import priv.sp.update._
import GameCardEffect._

object Entomologist extends ChangeTarget {
  import CardSpec._

  val giantAnt = new Creature("Giant Ant", Attack(5), 23, "All damage done to Giant Ant is reduced by 2.\nGiant Ant lowers the cost of other owner's Giant Ants by it while in play", reaction = new GiantReaction, effects = effects(Direct -> ant))
  val assassinWasp = new Creature("Assassin Wasp", AttackSources(Some(5), Vector(AssassinAttackSource)), 26, "When Summoned, Assassin Wasp summons 2/14 Wasp Drones* in its adjacent slots.\nAssassin Wasp gains a + 2 to it's attack for each Wasp Drone in play.", effects = effects(Direct -> assassin))

  val Entomologist: House = House("Entomologist", List(
    new Creature("Fire Beetle", Attack(3), 12, "When summoned, Fire Beetle deals 4 damage to it's opposite creature.\nWhen Fire Beetle dies it increases owner's Fire Power by 1.",
      reaction = new FireBeetleReaction,
      effects = effects(Direct -> beetle)),
    new Creature("Poisonpowder Moth", Attack(3), 14, "Each turn Poisonpowder Moth deals 1 damage to all opponent creatures & to opponent.\nWhen Poisonpowder Moth dies it deals 3 damage to all opponent creatures.", effects = effects(OnTurn -> damage(1, isAbility = true), OnTurn -> focus(damageCreatures(1, isAbility = true))), reaction = new MothReaction),
    Spell("Hivemind", "All caster's creatures attack target creature instead of their opposite creature's this turn.", inputSpec = Some(SelectTargetCreature), effects = effects(Direct -> hivemind)),
    Spell("Locust Swarm", "Summon a Locust Swarm onto a target opponent creature.\nTarget creature takes 8 damage each turn & opponent takes 4 damage each turn until target creatures dies.", inputSpec = Some(SelectTargetCreature), effects = effects(Direct -> locust)),
    giantAnt,
    assassinWasp,
    new Creature("Insect Hive", Attack(2), 30, "At the end of each owner's turn Insect Hive summons a 4/10 Insect Warrior* into a random owner's slot.", effects = effects(OnEndTurn -> hive)),
    new Creature("Red Mantis", Attack(7), 47, "When summoned, Red Mantis reduces all opponent's powers by 1.\nRed Mantis lowers by 1 the growth of opponent's power of it's opposite creature's power type.\nWhen Red Mantis dies it deals 5 damage to it's opposite creature.", effects = effects(Direct -> { env: Env ⇒
      env.otherPlayer.houses.incrMana(-1, 0, 1, 2, 3, 4)
    }, OnTurn -> mantis), reaction = new MantisReaction)), data = Targeting(), eventListener = Some(new CustomListener(new EntoEventListener)))

  Entomologist.initCards(Houses.basicCostFunc)

  private val drone = new Creature("Wasp Drone", Attack(2), 14, reaction = new DroneReaction)
  private val insect = new Creature("Insect Warrior", Attack(4), 10, "When Insect Warrior dies it increases owner's Insect/Entomologist Power by 1.", reaction = new InsectReaction)

  drone.cost = 0
  insect.cost = 1

  List(drone, insect).foreach { c ⇒
    c.houseIndex = Entomologist.houseIndex
    c.houseId = Entomologist.houseId
  }

  def beetle: Effect = { env: Env ⇒
    val selected = env.getOwnerSelectedSlot()
    selected.oppositeSlot inflict Damage(4, env, isAbility = true)
  }

  class FireBeetleReaction extends Reaction {
    override def onMyDeath(dead: Dead) {
      selected.focus()
      selected.player.houses.incrMana(1, 0)
    }
  }

  class MothReaction extends Reaction {
    override def onMyDeath(dead: Dead) {
      selected.focus()
      selected.otherPlayer.slots inflictCreatures Damage(3, Context(selected.playerId, Some(dead.slot.card), dead.num), isAbility = true)
    }
  }

  def hivemind = changeTarget

  def locust = { env: Env ⇒
    import env._
    val s = getTargetSelectedSlot()
    s toggle CardSpec.cursedFlag
    player addEffectOnce (OnEndTurn -> Locust(s.get.id))
  }

  def ant = { env: Env ⇒
    env.player addDescMod LowerGiantCostMod
  }

  class GiantReaction extends Reaction {
    override def selfProtect(d: Damage) = {
      d.copy(amount = math.max(0, d.amount - 2))
    }
    override def cleanUp() {
      selected.player removeDescMod LowerGiantCostMod
    }
  }

  case object LowerGiantCostMod extends DescMod {
    def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
      if (house.houseIndex < 4) cards
      else cards.map { c ⇒
        if (c.card == giantAnt) {
          c.copy(cost = math.max(0, c.cost - 2))
        } else c
      }
    }
  }

  private def assassin = { env: Env ⇒
    def spawnDroneAt(num: Int) {
      if (env.player.value.isInSlotRange(num)) {
        val slot = env.player.slots(num)
        if (slot.value.isEmpty) {
          slot add drone
        }
      }
    }
    spawnDroneAt(env.selected - 1)
    spawnDroneAt(env.selected + 1)
  }

  case object AssassinAttackSource extends AttackStateFunc {
    def apply(attack: Int, player: PlayerUpdate) = {
      val nbDrone = player.slots.slots.count { s ⇒
        s.value.isDefined && s.get.card == drone
      }
      attack + nbDrone * 2
    }
  }

  class DroneReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) { setAssassinDirty(selected.slots) }
    override def onMyRemove(dead: Option[Dead]) { setAssassinDirty(selected.slots) }
    def setAssassinDirty(slots: SlotsUpdate) {
      slots foreach { s ⇒
        if (s.get.card == assassinWasp) {
          s.attack.setDirty()
        }
      }
    }
  }

  def hive: Effect = { env: Env ⇒
    val openSlots = env.player.slots.getOpenSlots
    if (openSlots.nonEmpty) {
      val slot = openSlots(scala.util.Random.nextInt(openSlots.size))
      slot add insect
      slot.focus(blocking = false)
    }
  }

  class InsectReaction extends Reaction {
    override def onMyDeath(dead: Dead) {
      selected.player.houses.incrMana(1, 4)
    }
  }

  def mantis: Effect = { env: Env ⇒
    import env._
    val slot = getOwnerSelectedSlot()
    slot.oppositeSlot.value foreach { s ⇒
      env.otherPlayer.houses.incrMana(-1, s.card.houseIndex)
    }
  }

  class MantisReaction extends Reaction {
    override def onMyDeath(dead: Dead) {
      selected.oppositeSlot inflict Damage(5, Context(selected.playerId, Some(dead.slot.card), dead.num), isAbility = true)
    }
  }

  def getTargeting(player : PlayerUpdate) : Targeting = player.value.data.asInstanceOf[Targeting]
  def setTarget(player : PlayerUpdate, target : Option[Int] ) : Unit = player.updateData[Targeting](x ⇒ x.copy(target = target))

  class EntoEventListener extends ChangeTargetListener
}

case class Locust(id: Int) extends Function[Env, Unit] {
  def apply(env: Env) {
    (env.otherPlayer.slots findSlot id) match {
      case Some(s) ⇒
        val damage = Damage(8, Context(env.playerId, None), isSpell = true)
        val playerDamage = damage.copy(amount = 4)
        env.otherPlayer inflict playerDamage
        s inflict damage
      case None ⇒ env.player removeEffect (_ == this)
    }
  }
}

