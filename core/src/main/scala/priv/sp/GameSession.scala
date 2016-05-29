package priv.sp

import priv.sp.update._
import priv.util.TVar

trait GameSessionController {
  def endGame(msg : String) : Unit
  def disableSlots() : Unit
  def setCardEnabled(player : PlayerId, enabled : Boolean = true) : Unit
  def notifyPlayed(playerId : PlayerId, card : Option[Card]) : Unit
  def setPhase(player : PlayerId, phase : Option[String]): Unit
  def refresh(silent : Boolean) : Unit
  def addVisibleCard(player : PlayerId, card : Card) : Unit
  def waitForUser(c: TVar[Option[Command]]) : Unit
}

class GameSession(val server: GameServer, resources: GameResources) {

  @volatile var state = server.initState
  val sp = resources.sp
  val desc = server.desc
  val myPlayerId = other(server.playerId)
  val otherPlayerId = server.playerId
  val names = playerIds.map { id ⇒ if (id == myPlayerId) "me" else server.name }
  var gameLock = new priv.util.RichLock
  var controller : GameSessionController = null

  val updater = new GameStateUpdater(state, desc)
  persistUpdater() // bullshit for warp

  server.abort = { () ⇒
    endGame(owner)
  }

  def start() {
    server.resetSeed()
    persist(updater lift { u ⇒
      playerIds foreach { id ⇒
        u.players(id) applyEffects CardSpec.OnStart
      }
    })
    refresh()
    waitPlayer(server.startingPlayer)
  }

  private def waitPlayer(player: PlayerId) {
    def autoSkip[A](default: A): Option[A] = if (state.players(player).isDisabled) Some(default) else None

    if (player == server.playerId) {
      (autoSkip[Option[Option[Command]]](Some(None))
        orElse {
          controller.setCardEnabled(player)
          updater.resetRand()
          gameLock.waitFor[Option[Option[Command]]] { c ⇒
            server.waitNextCommand(c, state)
          }
        }).foreach { nextCommandOpt: Option[Option[Command]] ⇒
          nextCommandOpt match {
            case None ⇒ endGame(myPlayerId)
            case Some(nextCommand) ⇒
              nextCommand.foreach { c ⇒
                controller.addVisibleCard(player, c.card)
              }
              submit(nextCommand, player)
          }
        }
    } else {
      (autoSkip[Option[Command]](None)
        orElse gameLock.waitFor[Option[Command]](controller.waitForUser)) foreach { nextCommand ⇒
          server submitCommand nextCommand
          submit(nextCommand, player)
        }
    }
  }

  def endGame(player: PlayerId) {
    val msg = if (player == myPlayerId) "YOU WON" else (names(player) + " WON")
    controller.endGame(msg)
  }

  private def persist[A](stateFunc: GameState => (GameState, A)): A = {
    val result = stateFunc(state)
    state = result._1
    result._2
  }
  def persistState(newState: GameState) { state = newState }
  def persistUpdater() = persistState(updater.result) // crappy side effect

  def giveMeMana() {
    persist(updater.lift(_.players(owner).houses.incrMana(10, 0, 1, 2, 3, 4)))
    refresh()
  }


  private def submit(commandOption: Option[Command], playerId: PlayerId) = {
    println(playerId + " submit " + commandOption)
    controller.disableSlots()
    persist(updater.lift(_.players(playerId).submit(commandOption)))
    controller.notifyPlayed(playerId, commandOption.map(_.card))
    endOr {
      refresh()

      if (state.players(playerId).transitions.isEmpty) {
        playerIds.foreach(p => controller.setCardEnabled(p, false))
        controller.setPhase(playerId, None)
        run(playerId)
      } else {
        val t = persist(updater.lift { u ⇒
          u.players(playerId).popTransition.get
        })
        endOr {
          t match {
            case WaitPlayer(p, name) ⇒
              if (p != playerId) playerIds.foreach(p => controller.setCardEnabled(p, false))
              controller.setPhase(p, Some(name))
              waitPlayer(p)
          }
        }
      }
    }
  }

  private def endOr(f: ⇒ Unit) {
    updater.ended match {
      case Some(player) ⇒
        refresh()
        endGame(player)
      case _ ⇒ f
    }
  }

  private def run(playerId: PlayerId) {
    persist(updater.lift { u ⇒
      val p = u.players(playerId)

      endOr {
        println("run" + playerId)
        p.runSlots()
        persistUpdater()
        refresh()
        endOr {
          p applyEffects CardSpec.OnEndTurn
          p.slots.toggleRun()
          persistUpdater()
          endOr {
            val otherPlayer = p.otherPlayer
            otherPlayer.prepareNextTurn()
            otherPlayer applyEffects CardSpec.OnTurn
            persistUpdater()
            refresh(silent = true)
            endOr {
              waitPlayer(otherPlayer.id)
            }
          }
        }
      }
    })
  }

  private def refresh(silent: Boolean = false) = {
    controller.refresh(silent)
  }
}
