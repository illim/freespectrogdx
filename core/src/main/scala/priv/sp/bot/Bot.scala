package priv.sp.bot

import priv.sp._
import priv.sp.update._

class Knowledge(gameDesc: GameDesc, botPlayerId: PlayerId, knownCards: Set[(Card, Int)], val otherPlayerDesc: PlayerDesc) {
  val desc = ripPlayerState(gameDesc)
  /**
   * println("AI K :" + otherPlayerDesc.houses.map{ h =>
   * h.house.name + "/" + h.cards.toList
   * })
   */
  // bs
  def ripDescReader(gs: GameState) = {
    GameState(gs.players.zipWithIndex map {
      case (p, i) ⇒
        p.copy(desc = new DescReader(desc.players(i), p.desc.descMods))
    })
  }
  private def ripPlayerState(gameDesc : GameDesc) = {
    gameDesc.copy(players = gameDesc.players.updated(other(botPlayerId), otherPlayerDesc))
  }
}

object Bot {
  def loopWhile(settings: Settings)(f: ⇒ Boolean) = {
    val startTime = System.currentTimeMillis
    val end = startTime + settings.duration
    var i = 0
    var continue = true
    while (System.currentTimeMillis < end && continue) {
      continue = f
      i += 1
    }
    (System.currentTimeMillis() - startTime, i)
  }
}

case class BotContext(botPlayerId: PlayerId, start: GameState, settings: Settings) {
  val humanId = other(botPlayerId)
}

class BotKnowledge(
    gameDesc: GameDesc,
    val spHouses: Houses,
    botPlayerId: PlayerId) {

  var knownCards = Set.empty[(Card, Int)] // card and his index in house
  var k = generateK().get

  def updateKnowledge(command: Command, indexOfCardInHouse: Int) {
    import command._
    val c = (card, indexOfCardInHouse)
    if (!knownCards.contains(c)) {
      knownCards += c
      generateK(2).foreach { k = _ }
    }
  }

  def generateK(timeLimit: Int = Int.MaxValue) = {
    println("generating AI fake player")
    val start = System.currentTimeMillis
    val guess = new CardGuess(gameDesc, spHouses)
    guess.createAIPlayer(botPlayerId, knownCards, timeLimit) map { fakePlayerDesc ⇒
      println("generated k in " + (System.currentTimeMillis - start) + " ms")
      new Knowledge(gameDesc, botPlayerId, knownCards, fakePlayerDesc)
    }
  }

  def reset() {
    knownCards = Set.empty
    k = generateK().get
  }
}

class BotSimulator(val knowledge: BotKnowledge, val context: BotContext) {

  val updater = new GameStateUpdater(context.start, knowledge.k.desc)

  def simulateCommand(state: GameState, command: Command): (GameState, Transition) = {
    simulateCommand(state, command.player, Some(command))
  }

  def simulateCommand(state: GameState, playerId: PlayerId, commandOption: Option[Command]): (GameState, Transition) = {
    try {
      updater.lift { u ⇒
        val p = u.players(playerId)

        p submit commandOption
        u.flush()
        p.popTransition getOrElse {
          p.runSlots()
          if (u.ended.isEmpty) {
            p applyEffects CardSpec.OnEndTurn
            p.slots.toggleRun()
            val otherPlayer = p.otherPlayer
            otherPlayer.prepareNextTurn()
            if (u.ended.isEmpty) {
              otherPlayer applyEffects CardSpec.OnTurn
            }
          }
          WaitPlayer(other(playerId))
        }
      } (state)
    } catch {
      case t: Throwable ⇒
        println("Failed on " + commandOption + "/" + state)
        throw t
    }
  }
}

class BoardView(state: GameState, playerId: PlayerId) {
  val p = state.players(playerId)
  lazy val slots = p.slots
  lazy val openSlots = PlayerState.openSlots(p)
  lazy val otherp = state.players(other(playerId))
  lazy val otherOpenSlots = PlayerState.openSlots(otherp)
  lazy val otherSlots = otherp.slots
}

class Choices(cardStats: List[CardStats], settings: Settings) {

  def getNexts(state: GameState, playerId: PlayerId): Stream[Command] = {
    val board = new BoardView(state, playerId)
    import board._

    p.desc.get.houses.flatMap { houseDesc ⇒
      val houseState = p.houses(houseDesc.house.houseIndex)

      val cardDescs = houseDesc.cards.filter(_.isAvailable(houseState)).sortBy { c ⇒
        (!c.card.isSpell, -c.cost)
      } // try spell first(direct effects + may be less slot dependent)
      cardDescs.flatMap { cardDesc ⇒
        import cardDesc.card
        card.inputSpec match {
          case None ⇒ List(Command(playerId, card, None, cardDesc.cost))
          case Some(SelectOwner(f)) ⇒
            f(playerId, state).map { num ⇒
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
          case Some(SelectOwnerSlot) ⇒
            openSlots.map { num ⇒
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
          case Some(SelectOwnerCreature) ⇒
            slots.keys.map { num ⇒
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
          case Some(SelectTarget(f)) ⇒
            f(other(playerId), state).map { num ⇒
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
          case Some(SelectTargetSlot) ⇒
            otherOpenSlots.map { num ⇒
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
          case Some(SelectTargetCreature) ⇒
            otherSlots.keys.map { num ⇒
              Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
            }
        }
      }
    }(collection.breakOut)
  }

  import scala.util.Random

  def getRandomMove(state: GameState, playerId: PlayerId): Option[Command] = {
    val board = new BoardView(state, playerId)
    import board._

    val cardStat = cardStats(playerId)
    val cardOption = cardStat getRandomCard board

    cardOption flatMap { cardDesc ⇒
      import cardDesc.card
      card.inputSpec match {
        case None ⇒ Some(Command(playerId, card, None, cardDesc.cost))
        case Some(SelectOwner(f)) ⇒
          randHeadOption(f(playerId, state)).map { num ⇒
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
        case Some(SelectOwnerSlot) ⇒
          val (blockeds, unBlockeds) = openSlots.partition(otherp.slots.isDefinedAt _)
          (if (Random.nextFloat < settings.simBlockedSlotWeight) randHeadOption(blockeds) else randHeadOption(unBlockeds)).map { num ⇒
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
        case Some(SelectOwnerCreature) ⇒
          randHeadOption(state.players(playerId).slots.keys.toSeq).map { num ⇒
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
        case Some(SelectTarget(f)) ⇒
          randHeadOption(f(other(playerId), state)).map { num ⇒
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
        case Some(SelectTargetSlot) ⇒
          randHeadOption(PlayerState.openSlots(otherp)).map { num ⇒
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
        case Some(SelectTargetCreature) ⇒
          randHeadOption(state.players(other(playerId)).slots.keys.toSeq).map { num ⇒
            Command(playerId, card, Some(new SlotInput(num)), cardDesc.cost)
          }
      }
    }
  }

  def randHeadOption[A](s: Seq[A]): Option[A] = {
    if (s.isEmpty) None else Some(s(Random.nextInt(s.size)))
  }

}
