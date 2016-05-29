package priv.sp.house

import priv.sp._
import priv.sp.update._
import CardSpec._
import GameCardEffect._
import priv.util.FuncDecorators

/**
 * Introduced bullshit:
 * crossbowman -> doing something before dying = shitty code on removal
 */
class MoutainKing {

  val soldier = new Creature("Dwarven soldier", Attack(3), 12, "On entering the game increases attack of neighbors by 3 for 1 turn.\nHird: decreases attack of opposite creature by 1 for each\ndwarven soldier on the board.", effects = effects(Direct -> soldierEffect), reaction = new SoldierReaction)
  val shieldman = new Creature("Dwarven shieldman", Attack(3), 15, "Redirects to himself half of the damage dealt to neighbors.\nHird: cannot be killed with magic (at least 1 hp will be left).", reaction = new ShieldmanReaction)
  val runesmith = new Creature("Runesmith", AttackSources(Some(7), Vector(RuneAttackSource)), 24, "Runesmith can only be harmed by opposite creature if blocked.\nHird: +1 attack for each dwarf in the game (including himself).", reaction = new RuneReaction)
  val ballista = new Creature("Ballista", Attack(6), 34, "When enemy creature enters the game, halves its health and loses 10 health itself.\nHird: loses only 7 health on activating ability.", reaction = new BallistaReaction)
  val berserker = new Creature("Berserker", AttackSources(Some(6), Vector(BerserkerAttackSource)), 40, "When owner receives more than 4 damage attacks out-of-turn opposite slot.\nHird: +3 attack and damage dealt to berserker.", reaction = new BerserkerReaction)
  val moutainKing = new Creature("Moutain king", Attack(5), 45, "When allied dwarf enters the game, heals himself by 6 and permanently increases his attack by 2.\nWhen enters the game stuns strongest opponent creature.\nHird: reduces cost of dwarven cards by 1.", effects = effects(Direct -> moutain), reaction = new MountainReaction)
  val armourClad = new Creature("Armour-clad Dwarf", Attack(5), 23, "Reduces non-magical damage dealt to him by X\n(X = difference in level with opposite creature).\nHird: his ability applies to neighbors as well.", reaction = new ArmourReaction)

  val MoutainKing = House("Moutain King", List(
    soldier,
    shieldman,
    new Creature("Dwarven crossbowman", Attack(4), 17, "When attacks deals the same damage directly to opponent.\nHird: attacks out-of-turn right before death.", runAttack = new CrossbowAttack, reaction = new CrossbowReaction),
    armourClad,
    runesmith,
    ballista,
    berserker,
    moutainKing),
    eventListener = Some(new CustomListener(new MKEventListener)),
    description = "Hird:\nWhile dwarf stands side by side with other dwarf, he gets second ability")

  MoutainKing initCards Houses.basicCostFunc

  def soldierEffect = { env: Env ⇒
    import env._
    val bonus = AttackAdd(3)
    getOwnerSelectedSlot().filledAdjacents foreach (_.attack.add(bonus))
    player addEffect (OnEndTurn -> new CountDown(1, { e ⇒
      e.player.slots(selected).filledAdjacents foreach (_.attack.removeFirst(bonus))
    }))
  }

  def moutain = { env: Env ⇒
    import env._
    otherPlayer.slots.reduce(strongest _).foreach { s ⇒
      s toggle CardSpec.stunFlag
    }
  }

  class SoldierReaction extends Reaction {
    override def onMyRemove(dead: Option[Dead]) {
      val otherPlayer = selected.otherPlayer
      otherPlayer.getSlots.get(selected.num) match {
        case Some(s) if s.attackSources.sources.contains(SoldierLowerAttack) ⇒
          otherPlayer.slots(selected.num).attack removeFirst SoldierLowerAttack
        case _ ⇒
      }
      setSoldierOppAttackDirty(otherPlayer)
    }

    def setHird(s: SlotUpdate, b: Boolean, otherPlayer: PlayerUpdate) {
      val oppSlot = otherPlayer.slots(s.num)
      if (oppSlot.value.isDefined) {
        if (b) {
          if (!oppSlot.attack.has[SoldierLowerAttack.type]) {
            oppSlot.attack add SoldierLowerAttack
          }
        }
      }
      setSoldierOppAttackDirty(otherPlayer)
    }

    def setSoldierOppAttackDirty(otherPlayer: PlayerUpdate) {
      otherPlayer.slots foreach { s ⇒
        if (s.attack.has[SoldierLowerAttack.type]) {
          s.attack.setDirty()
        }
      }
    }
  }

  class RuneReaction extends Reaction {
    final override def selfProtect(d: Damage) = {
      if (selected.oppositeState.isDefined && (d.context.selected != selected.num || !d.context.card.exists(!_.isSpell))) {
        d.copy(amount = 0)
      } else d
    }
  }

  case object RuneAttackSource extends AttackSlotStateFunc {
    def apply(attack: Int, slot: SlotUpdate) = {
      if (slot.value.exists(_.data == Hird)) {
        val nbDwarf = slot.slots.filleds.count(_.get.card.houseId == MoutainKing.houseId)
        attack + nbDwarf
      } else attack
    }
  }

  case object SoldierLowerAttack extends AttackSlotStateFunc {
    def apply(attack: Int, slot: SlotUpdate) = {
      val oppSlots = slot.otherPlayer.getSlots
      oppSlots.get(slot.num) match {
        case Some(s) if s.card == soldier && s.data == Hird ⇒
          val nbSoldier = oppSlots.count(_._2.card == soldier)
          math.max(0, attack - nbSoldier)
        case _ ⇒ attack
      }
    }
  }

  class ShieldmanReaction extends Reaction {
    lazy val someShieldman = Some(shieldman)

    final override def onProtect(d: DamageEvent) = {
      import d._
      target match {
        case Some(num) if math.abs(num - selected.num) == 1
          && damage.context.card != someShieldman ⇒
          val context = Context(player.id, someShieldman, selected.num)
          val amount = d.damage.amount
          val samount = math.ceil(amount / 2.0).intValue
          selected inflict damage.copy(amount = samount, context = context)
          d.damage.copy(amount = amount - samount)
        case _ ⇒ d.damage
      }
    }

    override def selfProtect(d: Damage) = {
      val life = selected.get.life
      if (selected.get.data == Hird && d.isSpell && d.amount >= life) {
        d.copy(amount = life - 1)
      } else d
    }
  }

  class ArmourReaction extends Reaction {
    final override def selfProtect(d: Damage) = protectFromOpp(d, selected)

    final override def onProtect(d: DamageEvent) = {
      import d._
      d.target match {
        case Some(num) if selected.get.data == Hird && selected.get.card != armourClad
          && math.abs(selected.num - num) == 1 ⇒
          protectFromOpp(damage, player.slots(num))
        case _ ⇒ d.damage
      }
    }

    def protectFromOpp(d: Damage, slot: SlotUpdate) = {
      val oppSlotState = slot.oppositeState
      val levelDiff = oppSlotState.map(s ⇒ math.max(0, s.card.cost - slot.get.card.cost)) getOrElse 0
      if (!d.isEffect && levelDiff != 0) {
        d.copy(amount = math.max(0, d.amount - levelDiff))
      } else d
    }
  }

  class BallistaReaction extends Reaction {
    final override def onSummon(summoned: SummonEvent) {
      import summoned._
      if (selected.playerId != player.id) {
        val context = Context(selected.playerId, Some(ballista), selected.num)
        val damage = Damage(math.ceil(player.slots(num).get.life / 2.0).intValue, context, isAbility = true)
        selected.focus()
        player.slots(num) inflict damage
        val balSlot = player.otherPlayer.slots(selected.num)
        val damageAmount = if (balSlot.value.exists(_.data == Hird)) 7 else 10
        balSlot inflict Damage(damageAmount, context, isAbility = true)
      }
    }
  }

  class BerserkerReaction extends Reaction {
    var data = -1 // HACK
    def onPlayerDamage(amount: Int, slot: SlotUpdate) {
      if (amount > 4) {
        if (data == -1) {
          data = 0
          runSlot(slot)
          while (slot.value.isDefined && data > 0) {
            data -= 1
            runSlot(slot)
          }
          data = -1
        } else {
          data += 1
        }
      }
    }
    private def runSlot(slot: SlotUpdate) = {
      slot.player.runSlot(slot.num, slot.get)
    }
    override def selfProtect(d: Damage) = {
      if (selected.get.data == Hird) {
        d.copy(amount = d.amount + 3)
      } else d
    }
  }

  case object BerserkerAttackSource extends AttackSlotStateFunc {
    def apply(attack: Int, slot: SlotUpdate) = {
      if (slot.value.exists(_.data == Hird)) {
        attack + 3
      } else attack
    }
  }

  val lowerSpecialCostMod = LowerCostMod(Set(4))
  class MountainReaction extends Reaction {
    final override def onMyRemove(dead: Option[Dead]) {
      if (selected.get.data == Hird) {
        setHird(false, selected.player)
      }
    }
    final override def onSummon(summoned: SummonEvent) {
      import summoned._
      if (selected.playerId == player.id && card.houseId == MoutainKing.houseId) {
        selected heal 6
        selected.attack add AttackAdd(2)
      }
    }
    def setHird(b: Boolean, player: PlayerUpdate) {
      if (b) player addDescMod lowerSpecialCostMod
      else player removeDescMod lowerSpecialCostMod
    }
  }

  // crap
  class MKEventListener extends HouseEventListener {
    val moutainHouseId = MoutainKing.houseId
    def protect(slot: SlotUpdate, damage: Damage) = {
      player.slots.foldl(damage) { (acc, s) ⇒
        val sc = s.get.card
        if (sc.houseIndex == 4) {
          s.get.reaction onProtect DamageEvent(acc, Some(slot.num), player)
        } else acc
      }
    }

    def onDeath(dead: Dead) {
      val c = dead.card
      if (dead.player.id == player.id && c.houseId == moutainHouseId) {
        player.slots foreach { s ⇒
          if (s.num != dead.num) {
            val c = s.get.card
            if (c.houseId == moutainHouseId) {
              if (math.abs(s.num - dead.num) == 1) {
                val sym = s.num + (s.num - dead.num)
                if (!inSlotRange(sym)
                  || !player.slots(sym).value.exists(_.card.houseId == moutainHouseId)) {
                  setHird(s, false)
                }
              }
              s.get.reaction onDeath dead
            }
          }
        }
      }
    }
    def onAdd(slot: SlotUpdate) {
      if (slot.playerId == player.id) {
        if (slot.value.isEmpty) {
          println(slot.num + " is empty in " + slot.slots.value)
        }
        if (slot.get.card.houseId == moutainHouseId) {
          if (slot.filledAdjacents.count { s ⇒
            if (s.get.card.houseId == moutainHouseId) {
              if (s.get.data == null) {
                setHird(s, true)
              }
              true
            } else false
          } > 0) {
            setHird(slot, true)
          }
        }
      } else {
        player.getSlots.get(slot.num) match {
          case Some(s) if s.card == soldier && s.data == Hird ⇒
            slot.attack add SoldierLowerAttack
          case _ ⇒
        }
      }
    }
    def onPlayerDamage(damage: Damage) {
      player.slots.foreach { s ⇒
        val c = s.get.card
        if (c.houseIndex == 4) {
          s.get.reaction match {
            case br: BerserkerReaction ⇒ br.onPlayerDamage(damage.amount, s)
            case _                     ⇒
          }
        }
      }
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.slots.onDead = (FuncDecorators decorate p.slots.onDead) after (onDeath _)
      p.slots.slots foreach { slot ⇒
        slot.add = (FuncDecorators decorate slot.add) after (_ ⇒ onAdd(slot))
        slot.protect modifyResult (d ⇒ protect(slot, d))
      }
      p.onPlayerDamage = (FuncDecorators decorate p.onPlayerDamage) after { d: Damage =>
        onPlayerDamage(d)
      }
    }

    private def setHird(s: SlotUpdate, b: Boolean) {
      s.setData(if (b) Hird else null)
      s.get.reaction match {
        case sr: SoldierReaction  ⇒ sr.setHird(s, b, player.otherPlayer)
        case mr: MountainReaction ⇒ mr.setHird(b, player)
        case _ ⇒
          val c = s.get.card
          if (c == runesmith || c == berserker) {
            s.attack.setDirty()
          }
      }
    }

  }
}

class CrossbowAttack extends RunAttack {

  def apply(target: List[Int], d: Damage, player: PlayerUpdate) {
    val num = target.head
    val otherPlayer = player.otherPlayer
    val slot = otherPlayer.slots(num)
    otherPlayer inflict d
    if (slot.value.isDefined) {
      slot inflict d
    }
  }
}

class CrossbowReaction extends Reaction {
  // HACK should be on my death but the slot would already be empty
  final override def onMyRemove(dead: Option[Dead]) {
    if (selected.get.data == Hird) {
      selected setData null
      selected.player.runSlot(selected.num, selected.get)
    }
  }
}

case object Hird
