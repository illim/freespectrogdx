package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update._
import priv.util.FuncDecorators

class SB {

  val bounty = new Creature("Bounty hunter", Attack(4), 16,
    """Deals 2 damage to opposite creature when summoned.
When kill opposite creature, get 1/3 mana of his cost round up""",
    reaction = new BountyHunterReaction,
    effects = effects(Direct -> { env: Env ⇒
      env.focus()
      env.otherPlayer.slots(env.selected) inflict Damage(2, env, isAbility = true)
    }))
  val deathLetter = new Creature("Death letter", Attack(0), 3, """
Reduce damage to 1.
When die deals 15 damage to opposite creature.
(Can't be healed)""", reaction = new DLReaction)
  val maiko = new Creature("Maiko", Attack(2), 13,
    """Decrease by 1 attack of all creatures on board.
(Replaced by death letter after summoned)""", effects = effects(Direct -> maikoEffect), reaction = new MaikoReaction)

  val SB = House("Snowblood", List(
    new Creature("Tracker", Attack(3), 14, "When alive, next summoned creature is invincible one turn.", reaction = new TrackerReaction, data = java.lang.Boolean.FALSE, effects = effects(Direct -> initDataFalse)),
    bounty,
    maiko,
    Spell("Echo",
      "Each owner creature triggers his 'each turn' && 'direct' effects", effects = effects(Direct -> echo)),
    new Creature("Kojiro", Attack(5), 27,
      """Can only be summoned onto an existing creature.
Kojiro attack the turn he is summoned
Each turn deals 2 damage to opposite&aligned creatures.""", status = runFlag, effects = effects(OnTurn -> kojiro), inputSpec = Some(SelectOwnerCreature)),
    new Creature("War guide", Attack(5), 26,
      """When alive, next summoned creature deals damage equals to his attack to
opponent creatures.
Heal 1 life to aligned creatures when a creature is summoned in the pack""",
      reaction = new GuideReaction,
      data = java.lang.Boolean.FALSE,
      effects = effects(Direct -> initDataFalse)),
    new Creature("Janus", Attack(6), 25,
      """each turn drain (1/10maxlife) life from other side of the board
if there is a symetric creature to heal him by 2
For each creature drained, give one mana of the creature""", effects = effects(OnTurn -> janus)),
    new Creature("Amaterasu", Attack(7), 30, """When summoned, and when a creature is summoned apply Amaterasu rules
if fire deals 4 damage to opposite creature
if water increase lowest mana by 1
if air deals 2 damage to opponent
if earth heal 2 life to owner""", effects = effects(Direct -> amaterasu), reaction = new AmaterasuReaction)),
    eventListener = Some(new CustomListener(new SBEventListener)))

  SB initCards Houses.basicCostFunc
  SB.addAdditionalCards(deathLetter)
  deathLetter.cost = 3
  val maikoAbility = Ability(maiko, deathLetter)

  val someBounty = Some(bounty)
  class BountyHunterReaction extends Reaction {
    final override def onDeath(dead: Dead) {
      if (dead.player.id != selected.playerId) {
        dead.damage foreach { d ⇒
          if (d.context.selected == selected.num && d.context.card == someBounty) {
            selected.player.houses.incrMana(math.ceil(dead.card.cost / 3f).toInt, dead.card.houseIndex)
          }
        }
      }
    }
  }

  private def echo = { env: Env ⇒
    import env._
    player.slots foreach { s ⇒
      val c = s.get.card
      (c.effects(CardSpec.OnTurn).toList ++ c.effects(CardSpec.Direct)) foreach { f =>
        val env = new GameCardEffect.Env(playerId, updater)
        env.card = Some(c)
        env.selected = s.num
        f(env)
      }
    }
  }

  private def initDataFalse = {env: Env ⇒
    import env._
    player.slots(selected) setData java.lang.Boolean.FALSE
  }

  def maikoEffect: Effect = { env: Env ⇒
    import env._
    val malus = Lower1Attack(selected)
    player.slots foreach malus.temper
    otherPlayer.slots foreach malus.temper
    player addDescMod maikoAbility
  }

  class MaikoReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) = {
      if (slot != selected) {
        val malus = Lower1Attack(selected.num)
        slot.attack add malus
      }
    }
    final override def onRemove(slot: SlotUpdate) = {
      val malus = Lower1Attack(selected.num)
      slot.attack removeFirst malus
    }
    final override def cleanUp(): Unit = {
      val malus = Lower1Attack(selected.num)
      def removeMalus(s: SlotUpdate) { s.attack removeFirst malus }
      selected.player.slots foreach removeMalus
      selected.otherPlayer.slots foreach removeMalus
    }
  }

  class DLReaction extends Reaction {
    override def heal(amount: Int): Unit = {}
    override def inflict(damage: Damage): Unit = {
      super.inflict(damage.copy(amount = 1))
    }

    override def onMyDeath(dead: Dead): Unit = {
      val d = Damage(15, Context(selected.playerId, Some(dead.card), selected.num), isAbility = true)
      selected.oppositeSlot inflict d
    }
  }

  private def kojiro = { env: Env ⇒
    import env._
    val aligneds = getAligneds(otherPlayer.slots, selected)
    if (aligneds.nonEmpty) {
      getOwnerSelectedSlot.focus()
      val d = Damage(2, env, isAbility = true)
      aligneds foreach { s ⇒
        s inflict d
      }
    }
  }

  def getAligneds(slots: SlotsUpdate, selected: Int) = {
    val (aligneds, found, _) = slots.slots.foldLeft((List.empty[SlotUpdate], false, false)) {
      case (old @ (acc, found, end), s) ⇒
        if (end) old else {
          if (s.value.isEmpty) {
            if (found) (acc, true, true) else (Nil, false, false)
          } else {
            (s :: acc, (s.num == selected) || found, false)
          }
        }
    }
    if (found) aligneds else Nil
  }

  class GuideReaction extends Reaction with OnSummonable {
    final def onSummoned(slot: SlotUpdate) = {
      if (selected != slot) {
        if (!selected.get.data.asInstanceOf[Boolean]) {
          slot.value foreach { s ⇒
            val d = Damage(s.attack, Context(slot.playerId, Some(s.card), slot.num), isAbility = true)
            selected.otherPlayer.slots inflictCreatures d
            selected setData java.lang.Boolean.TRUE
          }
        }
        val aligneds = getAligneds(selected.slots, selected.num)
        if (aligneds contains (slot)) {
          aligneds foreach (_.heal(1))
        }
      }
    }
  }

  private def janus = { env: Env ⇒
    import env._
    val filleds = player.slots.filleds
    val (draineds, healeds) = if (selected > 2) {
      filleds partition (_.num < 3)
    } else {
      filleds partition (_.num > 2)
    }
    var hasDrained = false
    draineds foreach { slot ⇒
      slot.value foreach { s ⇒
        healeds.find(_.num == 5 - slot.num) foreach { h ⇒
          val d = Damage(math.ceil(s.card.life / 10f).intValue, env, isAbility = true)
          slot drain d
          player.houses.incrMana(1, s.card.houseIndex)
          hasDrained = true
          h heal 2
        }
      }
    }
    if (hasDrained) {
      getOwnerSelectedSlot.focus()
    }
  }

  def applyAmaterasuRules(selected: SlotUpdate, slot: SlotUpdate) {
    slot.get.card.houseIndex match {
      case 0 ⇒
        val d = Damage(4, Context(selected.playerId, Some(selected.get.card), selected.num), isAbility = true)
        slot.oppositeSlot inflict d
      case 1 ⇒
        selected.player.value.houses.zipWithIndex.sortBy(_._1.mana).headOption.foreach {
          case (_, idx) ⇒
            selected.player.houses.incrMana(1, idx)
        }
      case 2 ⇒
        val d = Damage(2, Context(selected.playerId, Some(selected.get.card), selected.num), isAbility = true)
        selected.otherPlayer inflict d
      case 3 ⇒
        selected.player heal 2
      case _ ⇒ ()
    }
  }

  def amaterasu = { env: Env ⇒
    import env._
    val selected = getOwnerSelectedSlot
    player.slots foreach { s ⇒
      if (s.get.card.houseIndex < 4) {
        s.focus()
        applyAmaterasuRules(selected, s)
      }
    }
  }

  class AmaterasuReaction extends Reaction with OnSummonable {
    final def onSummoned(slot: SlotUpdate) = {
      applyAmaterasuRules(selected, slot)
    }
  }

  class SBEventListener extends HouseEventListener with OppDeathEventListener {
    def onEnemyAdd(slot: SlotUpdate): Unit = {
      player.slots foreach { s ⇒
        s.get.reaction match {
          case mk: MaikoReaction ⇒ mk onAdd slot
          case _                 ⇒ ()
        }
      }
    }

    def onSummon(slot: SlotUpdate): Unit = {
      player.slots.foreach { s ⇒
        s.get.reaction match {
          case os: OnSummonable ⇒ os onSummoned slot
          case _                ⇒ ()
        }
      }
    }

    override def init(p: PlayerUpdate): Unit = {
      super.init(p)
      p.otherPlayer.slots.slots foreach { slot ⇒
        slot.add = (FuncDecorators decorate slot.add) after { _ ⇒ onEnemyAdd(slot) }
      }
      p.submitCommand = (FuncDecorators decorate p.submitCommand) after { c ⇒
        c.card match {
          case creature: Creature ⇒
            c.input foreach { i ⇒ onSummon(player.slots(i.num)) }
          case _ ⇒
        }
      }
    }
  }
}

trait OnSummonable {
  def onSummoned(slot: SlotUpdate)
}

class TrackerReaction extends Reaction with OnSummonable {
  final def onSummoned(slot: SlotUpdate) = {
    // FIXME weird case where wall of flame kill bennu, and bennu kill wall of flame before this is called
    selected.value foreach { state =>
      slot.value foreach { s => // hack for retaliator who can kill f2 before f2 becoming invincible
        if (!state.data.asInstanceOf[Boolean] && selected != slot) {
          slot toggle invincibleFlag
          slot.player addEffectOnce (OnTurn -> RemoveInvincible(s.id))
          selected setData java.lang.Boolean.TRUE
        }
      }
    }
  }
}

case class Lower1Attack(id: Int) extends AttackFunc {
  def apply(attack: Int) = math.max(0, attack - 1)

  def temper(s: SlotUpdate) : Unit = {
    s.attack add this
  }
}
