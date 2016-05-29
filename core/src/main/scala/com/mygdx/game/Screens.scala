package com.mygdx.game

import scala.util.control.NonFatal
import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.{Game, Gdx, ScreenAdapter}
import com.mygdx.game.actor.Repere
import com.mygdx.game.gui.{GameSettings, StartBoard}
import priv.sp.GameResources

// common resources of the screens
class Screens(val game : Game) {
  val screenResources       = new ScreenResources
  val gameResources         = new GameResources
  val repere                = new Repere(screenResources)
  var lastE                 = Option.empty[Throwable]
  val startScreen           = new StartScreen(this)
  val gameScreen            = new GameScreen(this)

  import screenResources._

  def resize (width : Int, height : Int) {
    stage.getViewport.update(width, height, true)
  }

  @inline def render (delta : Float): Unit = {
    try {
      beforeProcess()
      stage act delta

      Gdx.gl glClear GL20.GL_COLOR_BUFFER_BIT
      stage.draw()
      engine update delta
      lastE = None
    } catch {
      case NonFatal(e) =>
        if (! lastE.contains(e) ) {
          lastE = Some(e)
          e.printStackTrace()
        }
    }
  }

  def dispose(): Unit = {
    screenResources.dispose()
    gameResources.dispose()
  }
}

class StartScreen(screens : Screens) extends ScreenAdapter {
  import screens._
  val startBoard = new StartBoard(screens)

  startBoard.buttonPanel.newButton addListener onClick {
    gameScreen.select()
    new LocalGameScreenContext(gameScreen)
  }
  startBoard.buttonPanel.settingsButton addListener onClick {
    screenResources.stage addActor new GameSettings(gameResources, screenResources)
  }

  def select() : Unit = {
    screenResources.clear()
    screenResources.stage addActor startBoard.panel
    game setScreen this
  }

  override def render (delta : Float): Unit = screens.render(delta)

  override def resize (width : Int, height : Int) = screens.resize(width, height)
  override def dispose(): Unit = screens.dispose()
}


class GameScreen(val screens : Screens) extends ScreenAdapter {
  import screens._

  override def render (delta : Float): Unit = screens.render(delta)

  def resetScreen[A](f : => A) : A = {
    screenResources.engine.getSystems.toArray.foreach(_.setProcessing(false))
    // FIXME should wait here
    screenResources.clear()
    screenResources.stage addActor repere
    val res = f
    screenResources.engine.getSystems.toArray.foreach(_.setProcessing(true))
    res
  }

  def returnToStart() = {
    resetScreen(())
    startScreen.select()
  }

  def select() : Unit = {
    resetScreen(())
    game setScreen this
  }

  override def resize (width : Int, height : Int) = screens.resize(width, height)
  override def dispose(): Unit = screens.dispose()

}

