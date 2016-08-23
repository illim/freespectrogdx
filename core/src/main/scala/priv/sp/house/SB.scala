package priv.sp.house

import priv.sp.CardSpec._
import priv.sp.GameCardEffect._
import priv.sp._
import priv.sp.update._
import priv.util.FuncDecorators

class SB {

  val bounty = new Creature("snowblood.hunter", Attack(4), 16,
    I18n("snowblood.hunter.description"),
    reaction = new BountyHunterReaction,
    effects = effects(Direct -> { env: Env ⇒
      env.focus()
      env.otherPlayer.slots(env.selected) inflict Damage(2, env, isAbility = true)
    }))
  val deathLetter = new Creature("snowblood.letter", Attack(0), 3, I18n("snowblood.letter.description"), reaction = new DLReaction)
  val maiko = new Creature("snowblood.maiko", Attack(2), 20,
    I18n("snowblood.maiko.description"), 
	mod = Some(new SpellMod(x ⇒ x + 1)),
	effects = effects(Direct -> maikoEffect), reaction = new MaikoReaction)

  val SB = House("snowblood", List(
    new Creature("snowblood.tracker", Attack(3), 14, I18n("snowblood.tracker.description"), reaction = new TrackerReaction, data = java.lang.Boolean.FALSE, effects = effects(Direct -> initDataFalse)),
    bounty,
    maiko,
    Spell("snowblood.echo",
      I18n("snowblood.echo.description"), effects = effects(Direct -> echo)),
    new Creature("snowblood.kojiro", Attack(5), 27,
      I18n("snowblood.kojiro.description"), status = runFlag, effects = effects(OnTurn -> kojiro), inputSpec = Some(SelectOwnerCreature)),
    new Creature("snowblood.guide", Attack(10), 26,
      I18n("snowblood.guide.description"),
      reaction = new GuideReaction,
      data = java.lang.Boolean.FALSE,
      effects = effects(Direct -> initDataFalse)),
    new Creature("snowblood.janus", Attack(6), 25,
      I18n("snowblood.janus.description"), effects = effects(OnTurn -> janus)),
    new Creature("snowblood.amaterasu", Attack(7), 61, I18n("snowblood.amaterasu.description"), effects = effects(Direct -> amaterasu), reaction = new AmaterasuReaction)),
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
	//player addTransition WaitPlayer(playerId, "Echo phase") // было + 1 доп действие
	 env.otherPlayer addDescMod SkipTurn
	 env.otherPlayer addEffectOnce (OnEndTurn -> new Unfreeze(true))
	 
  }
  
 class Unfreeze(chain: Boolean) extends Function[Env, Unit] {
    def apply(env: Env) {
      import env._
      player removeDescMod SkipTurn
      player removeEffect (_.isInstanceOf[Unfreeze])
	  
	  if (chain) {
        otherPlayer addDescMod HideEchoMod
        otherPlayer addEffectOnce (OnEndTurn -> UnMod(HideEchoMod))
      }
    }
  }
  
  val echoCard = SB.cards(3)
  case object HideEchoMod extends DescMod {
	  def apply(house: House, cards: Vector[CardDesc]): Vector[CardDesc] = {
		if (house.houseIndex != 4) cards
		else 
			cards.map { c ⇒
			if (c.card == echoCard) {
			  c.copy(enabled = false)
			} else c
		  }
	  }
	}

  private def initDataFalse = {env: Env ⇒
    import env._
    player.slots(selected) setData java.lang.Boolean.FALSE
  }

  def maikoEffect: Effect = { env: Env ⇒
    import env._
    val malus = Higher1Attack(selected)
    player.slots foreach malus.temper
    otherPlayer.slots foreach malus.temper
    player addDescMod maikoAbility
  }

  class MaikoReaction extends Reaction {
    final override def onAdd(slot: SlotUpdate) = {
      if (slot != selected) {
        val malus = Higher1Attack(selected.num)
        slot.attack add malus
      }
    }
    final override def onRemove(slot: SlotUpdate) = {
      val malus = Higher1Attack(selected.num)
      slot.attack removeFirst malus
    }
    final override def cleanUp(): Unit = {
      val malus = Higher1Attack(selected.num)
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
      val d = Damage(5, env, isAbility = true)
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

case class Higher1Attack(id: Int) extends AttackFunc {
  def apply(attack: Int) = math.max(0, attack + 1)

  def temper(s: SlotUpdate) : Unit = {
    s.attack add this
  }
}
