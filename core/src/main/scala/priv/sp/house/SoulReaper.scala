package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect.Env
import priv.sp._
import priv.sp.update.{SlotUpdate, PlayerUpdate, HouseEventListener}
import priv.util.FuncDecorators

object SoulReaper {

  val initData = SoulReaperData()

  val damnation = Spell("Damnation", "Target creature and deal 3 damage to it.\n" +
    "If the creature dies because of this spell increase the number of souls by x \n" +
    "where x is equal to creatures cost (max. damage 8)",
    inputSpec = Some(SelectTargetCreature),
    effects   = effects(Direct -> damn))

  val onSlaught = Spell("Onslaught of souls", "Deal x damage to all opponents creatures where x is equal to the number of souls you have." +
    "After this spell is used owner does not receive souls for creatures killed by this spell and " +
    "the number of souls that owner has becomes 0",
    effects   = effects(Direct -> onSlaughtEffect))

  val death = Spell("Death Sentence", "Destroy all creatures on the field.\n" +
    "For every destroyed creature owner receive 2 souls instead of 1",
    effects = effects(Direct -> deathSentence))

  val SoulReaper: House = House("Soul Reaper", List(
    damnation,

    Spell("Dark Passage", "select 1 target creature. This creature and its neighboring creatures are healed with " +
      "x life where x is equal to the number of souls you have (max. heal 12 life)",
      inputSpec = Some(SelectOwnerCreature),
      effects   = effects(Direct -> pass)),

    Spell("Eternal Rage", "select 1 target creature this creature stays active on the field 2 more turns after death",
      inputSpec = Some(SelectOwnerCreature),
      effects   = effects(Direct -> eternalRage)),

    onSlaught,

    Spell("Tribute", "Select 1 target owner creature and destroy it owner gets x life " +
      "where x is equal with the life of the destroyed creature (max life 36)",
      inputSpec = Some(SelectOwnerCreature),
      effects   = effects(Direct -> tribute)),

    Spell("Furious souls", "Deal x+7 damage to opponent where x is equal to owners number of souls." +
      "After this spell is used souls number is reduced to 0",
      effects = effects(Direct -> furious)),

    death,

    Spell("Soul Shackles", "Target creature cannot attack and its ability is cancelled. " +
      "His slot cannot be used until the end of the duel.\n" +
      "For every turn the trapped creature remains on the field, the owner will get 1 soul.\n" +
      "If owner summons something in front of that slot after the opponents creature is destroyed, " +
      "that creature will receive 6 damage every turn",
      inputSpec = Some(SelectTargetCreature),
      effects   = effects(Direct -> shackle))
  ),
  data = initData,
  eventListener = Some(new CustomListener(new SoulReaperListener)),
  description = "When a creature on the field dies owner gets 1 soul. " +
    "For every soul that owner has all damage dealt by his special spells is increased by 1 (1 soul=1 damage)")

  SoulReaper initCards Houses.basicCostFunc

  def shackle = { env : Env =>
    import env._
    val slot = getTargetSelectedSlot()
    slot.value foreach { s =>
      slot remove None
      Warp.bridle(s.copy(attack = 0, attackSources = AttackSources(Some(0))), slot)
      otherPlayer blockSlot selected
      player addEffect (OnTurn -> new ShackleEffect(selected))
    }
  }

  class ShackleEffect(num : Int) extends Function[Env, Unit] {
    def apply(env : Env) = {
      val slot = env.player.slots(num)
      val oppSlot = slot.oppositeSlot
      if (oppSlot.value.isDefined) {
        env.player.updateData[SoulReaperData](data => data.copy(x = data.x + 1))
      } else if (slot.value.isDefined) {
        slot inflict Damage(6, env, isAbility = true)
      }
    }
  }

  def deathSentence = { env : Env =>
    def kill(slot : SlotUpdate): Unit = {
      slot.overridableDestroy()
      env.player.updateData[SoulReaperData](data => data.copy(x = data.x + 1))  // 1 already count in listener
    }
    env.player.slots foreach kill
    env.otherPlayer.slots foreach kill
  }

  def furious = { env : Env =>
    val x = getX(env.player)
    env.player setData initData
    env.otherPlayer inflict Damage(x + 7, env, isSpell = true)
  }

  def tribute = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    val life = slot.get.life
    env.player heal math.min(life, 36)
    slot.destroy()
  }

  def onSlaughtEffect = { env : Env =>
    env.otherPlayer.slots inflictCreatures Damage(getX(env.player), env, isSpell = true)
    env.player setData initData
  }

  def eternalRage = { env : Env => env.getOwnerSelectedSlot() setData Eternal }

  def pass = { env : Env =>
    val slot = env.getOwnerSelectedSlot()
    val x = math.min(12, getX(env.player))
    slot heal x
    slot.filledAdjacents foreach (_ heal x)
  }

  def damn = { env : Env =>
    val slot = env.getTargetSelectedSlot()
    val x = getX(env.player)
    slot inflict Damage(math.min(8, 3 + x), env, isSpell = true)
  }

  class SoulReaperListener extends HouseEventListener {
    val someOnslaught = Some(onSlaught)
    val someDamn = Some(damnation)

    def reactDead(playerId : PlayerId, dead : Dead) : Unit = {
      val isAttackFromOwner = dead.damage.exists(_.context.playerId == playerId)
      if (!(isAttackFromOwner && dead.damage.exists(_.context.card == someOnslaught))) {
        player.updateData[SoulReaperData](data => data.copy(x = data.x + 1))
        if (isAttackFromOwner && dead.damage.exists(_.context.card == someDamn)) {
          player.updateData[SoulReaperData](data => data.copy(x = data.x + dead.card.cost))
        }
      }
    }

    def checkEternal(s : SlotUpdate) = {
      val isEternal = s.get.data == Eternal
      if (isEternal) {
        s.value foreach { slotState =>
          if (slotState.life > 0) {
            s write Some(slotState.copy(life = 0))
            player addEffect (OnEndTurn -> new CountDown(2, { env: Env ⇒
              env.player.slots.findSlot(slotState.id) foreach (_.destroy())
            }))
          }
        }
      }
      isEternal
    }

    override def init(p: PlayerUpdate) {
      super.init(p)
      p.slots.onDead = (FuncDecorators decorate p.slots.onDead) after (dead => reactDead(p.id, dead))
      p.otherPlayer.slots.onDead = (FuncDecorators decorate p.otherPlayer.slots.onDead) after (dead => reactDead(p.id, dead))
      p.slots.slots foreach { slot =>
        slot.delayedDestroy = (FuncDecorators decorate slot.delayedDestroy) update { f =>
          { d: Damage =>
            if (!checkEternal(slot)) {
              f(d)
            }
          }
        }
        slot.overridableDestroy = (FuncDecorators decorate slot.overridableDestroy) update { f =>
          { () =>
            if (!checkEternal(slot)) {
              f()
            }
          }
        }
      }
    }
  }

  @inline def getData(p : PlayerUpdate) = {
    p.pstate.data match {
      case s : SoulReaperData => s
      case _ => initData
    }
  }
  @inline def getX(p : PlayerUpdate) = getData(p).x

  case object Eternal
}

case class SoulReaperData(x : Int = 0)