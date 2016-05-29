package com.mygdx.game

import com.mygdx.game.net.RemoteOpponent
import priv.sp.Local
import priv.util.Utils._

trait GameScreenContext {

  def releaseLocks(gameInit : GameInit) = {
    gameInit.session.gameLock.release()
    gameInit.listener.lock.release()
  }
}

class RemoteGameScreenContext(gameScreen: GameScreen, opponent : RemoteOpponent) extends GameScreenContext {
  import gameScreen.screens._
  import net._
  val gameInit = new GameInit(screenResources, gameResources, opponent)
  gameInit.userMenu.clearListeners()

  gameInit.userMenu.surrenderButton addListener onClick {
    opponent.client send Message(Header(MessageType.ExitDuel))
    releaseLocks(gameInit)
    gameScreen.returnToStart()
  }
}

class LocalGameScreenContext(gameScreen: GameScreen) extends GameScreenContext {
  import gameScreen.screens._
  var currentGame = createGame()

  def createGame(): GameInit = gameScreen.resetScreen {
    if (currentGame != null) {
      releaseLocks(currentGame)
    }
    val gameInit = new GameInit(screenResources, gameResources, new Local(gameResources))
    screenResources.disconnectIfNeeded()

    gameInit.userMenu.newButton addListener onClick {
      currentGame = createGame()
    }
    gameInit.userMenu.surrenderButton addListener onClick {
      releaseLocks(currentGame)
      gameScreen.returnToStart()
    }
    localInit(gameInit)
  }

  def localInit(gameInit : GameInit) = {
    new LocalGameInit(gameInit)
    gameInit
  }

  class LocalGameInit(gameInit : GameInit) {
    import gameInit._

    userMenu.restartButton.addListener(onClick {
      println("restart")
      switchNewGame {
        session.server.reset()
      }
    })

    def switchNewGame(f : => Unit) {
      screenResources.engine.getSystems.toArray.foreach(_.setProcessing(false))
      board.cardPanels foreach (_.setEnabled(false))
      session.gameLock.release()
      session.gameLock = new priv.util.RichLock
      listener.lock.release()
      listener.lock = new priv.util.RichLock
      screenResources.engine.removeAllEntities()
      f
      screenResources.engine.getSystems.toArray.foreach(_.setProcessing(true))
      gameResources.gameExecutor submit runnable {
        session persistState session.server.initState
        session.start()
      }
    }
  }

}

