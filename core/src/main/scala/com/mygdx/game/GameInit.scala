package com.mygdx.game

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.scenes.scene2d.actions.{MoveToAction, TemporalAction}
import com.badlogic.gdx.scenes.scene2d.ui.Image
import com.badlogic.gdx.scenes.scene2d.{Group, Actor}

import scala.concurrent.ExecutionContext.Implicits.global
import collection.JavaConverters._
import com.badlogic.gdx.{InputMultiplexer, Input, InputAdapter, Gdx}
import com.mygdx.game.gui._
import priv.sp._
import priv.sp.update.UpdateListener
import priv.util.{RichLock, TVar}
import priv.util.Utils._

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class GameInit(val screenResources : ScreenResources,
               val gameResources : GameResources,
               val server : GameServer) {

  val session          = new GameSession(server, gameResources)
  val descriptionPanel = new DescriptionPanel(screenResources)
  val historyPanel     = new DescriptionPanel(screenResources, Color.GRAY)
  val userMenu         = new ButtonPanel(screenResources)
  val selectedEffect   = new SelectedEffectActor(screenResources)
  val hoveredActor     = new HoveredActor(screenResources)
  val slotPanels = playerIds map { playerId =>
    new SlotPanel(playerId, session, descriptionPanel, screenResources)
  }
  val cardPanels = playerIds map { playerId =>
    new CardPanel(playerId, session, descriptionPanel, selectedEffect, hoveredActor, screenResources)
  }
  val background      = new Background(screenResources)
  val board           = new Board(session.myPlayerId, slotPanels, cardPanels, descriptionPanel, historyPanel, userMenu, screenResources)
  val commandRecorder = new CommandRecorder(session, board)
  session.controller  = new UserGameController(session, board, commandRecorder, screenResources)
  val listener        = new GameUpdateListener(board, session, screenResources)
  session.updater.updateListener = listener

  screenResources.stage addActor background.background
  screenResources.stage addActor selectedEffect
  screenResources.stage addActor hoveredActor
  screenResources.stage addActor board.panel
  slotPanels foreach (_.init(commandRecorder))
  cardPanels foreach (_.init(commandRecorder))
  background setBackground session.state.players(session.server.startingPlayer).desc.get.houses(4).house

  userMenu.skipButton.addListener(onClick {
    println("skip")
    if (session.updater.ended.isEmpty && commandRecorder.cont.isDefined) {
      commandRecorder.skip()
    }
  })

  var isDebug = false
  Gdx.input.setInputProcessor(
    new InputMultiplexer(Gdx.input.getInputProcessor,
      screenResources.stage,
      new InputAdapter(){
        override def keyDown(k : Int) = {
          handleKey(k)
        }

        override def scrolled(amount : Int) = {
          scroll(-30 * amount)
          true
        }

        private var y = 0
        private var h = 768
        private def scroll(delta : Int) = {
          val newy = y + delta
          val dy =
            if (newy<0) -y
            else if (newy > 2 *h) (2*h -y)
            else delta
          y = y + dy
          screenResources.stage.getCamera.translate(0, dy, 0)
        }
      }))

  gameResources.gameExecutor submit runnable(session.start())

  def setDebug(group : Group) : Unit = {
    group setDebug isDebug
    group.getChildren.asScala foreach {
      case g : Group => setDebug(g)
      case a : Actor => a.setDebug(isDebug)
    }
  }

  def handleKey(k : Int) : Boolean= {
    if (k == Input.Keys.F5) {
      Gdx.app.log("input", "reload resources")
      screenResources.reload()
      true
    } else if (k == Input.Keys.F6) {
      isDebug = !isDebug
      Gdx.app.log("input", "set debug " + isDebug)
      setDebug(board.panel)
      true
    } else if (k == Input.Keys.F8) {
      Gdx.app.log("input", "give mana")
      session.giveMeMana()
      true
    } else if (k == Input.Keys.F7) {
      println(session.state)
      true
    } else false
  }
}


class UserGameController(game : GameSession, board : Board, commandRecorder : CommandRecorder, resources : ScreenResources)
  extends GameSessionController {

  def endGame(msg : String) = {
    resources addEndMessage msg
    commandRecorder.cont foreach (_.set(None))
  }
  def disableSlots() : Unit = {
    board.slotPanels.foreach(_.disable())
  }
  def setCardEnabled(player : PlayerId, enabled : Boolean) : Unit = {
    board.cardPanels(player).setEnabled(enabled)
  }

  def notifyPlayed(playerId : PlayerId, card : Option[Card]) : Unit = {
    board.historyPanel.update(card.map(c => Description.cardToDesc(game.state, playerId, c)))
  }

  def setPhase(player : PlayerId, phase : Option[String]): Unit = {
    board.slotPanels(player).lifeLabel setPhase phase
  }

  def refresh(silent : Boolean) : Unit = {
    resources.beforeProcess invoke {
      resources.slotSystem.synchronizeState(game, resources, board)
      board refresh silent
    }
  }

  def addVisibleCard(player : PlayerId, card : Card) : Unit = {
    board.cardPanels(player) addVisibleCard card
  }

  def waitForUser(c: TVar[Option[Command]]) = {
    commandRecorder.startWith(c) {
      board.cardPanels(commandRecorder.game.myPlayerId) setEnabled true
    }
  }
}


class GameUpdateListener(board : Board, game : GameSession, resources : ScreenResources) extends UpdateListener {
  val spellCast = new SpellCast(board, game, resources)
  import board._
  import game._

  // TODO probably better to recreate the listener instead of recreating a lock when restart
  var lock = new RichLock()

  def waitAction(actor : Actor) = {
    lock waitLock { lock =>
      promAction(actor).future onComplete { _ =>
        lock synchronized {
          lock.notifyAll()
        }
      }
    }
  }

  def focus(num: Int, playerId: PlayerId, blocking: Boolean) : Unit = {
    resources.slotSystem
      .findEntity(num, playerId)
      .foreach { case (_, slot) =>
        if (blocking) waitAction(slot.focus())
        else slot.focus()
      }
  }

  def move(num: Int, dest: Int, playerId: PlayerId, destPlayerId : PlayerId) : Unit = {
    val slotOption = resources.beforeProcess invoke {
      resources.slotSystem
        .findEntity(num, playerId)
        .map { case (_, slot) =>
          val target = getCoord(slotPanels(destPlayerId).slots(dest).group)
          val move = new MoveToAction()
          move.setPosition(target.x + 15, target.y + 15)
          move setDuration 1
          slot.group addAction move
          slot.slotnum = dest
          slot.playerId = destPlayerId
          slot
        }
    }
    slotOption foreach { slot => waitAction(slot.group) }
  }

  def swap(num: Int, dest: Int, playerId: PlayerId, destPlayerId : PlayerId) : Unit = {
    val slotOption = resources.beforeProcess invoke {
      for {
        (_, srcEntity) <- resources.slotSystem.findEntity(num, playerId)
        (_, destEntity) <- resources.slotSystem.findEntity(dest, destPlayerId)
      } yield {
        val src = getCoord(slotPanels(playerId).slots(num).group)
        val target = getCoord(slotPanels(destPlayerId).slots(dest).group)
        val move = new MoveToAction()
        move.setPosition(target.x + 15, target.y + 15)
        move setDuration 1
        srcEntity.group addAction move
        srcEntity.playerId = destPlayerId
        srcEntity.slotnum = dest

        val moveDest = new MoveToAction()
        moveDest.setPosition(src.x + 15, src.y + 15)
        moveDest setDuration 1
        destEntity.group addAction moveDest
        destEntity.slotnum = num
        destEntity.playerId = playerId
        srcEntity
      }
    }
    slotOption foreach { slot => waitAction(slot.group) }
  }

  def runSlot(num: Int, playerId: PlayerId) : Unit = {
    resources.slotSystem
      .findEntity(num, playerId)
      .foreach { case (_, slot) => waitAction(slot.run()) }
  }

  def summon(slotnum: Int, slotState: SlotState, playerId: PlayerId) : Unit = {
    val (slotComponent, old) = resources.beforeProcess invoke {
      val source = board.cardPanels(playerId)
        .findCardButtonOf(slotState.card)
        .fold[Vector2](new Vector2(0, 600))(x => getCoord(x.group))
      val (entity, slotComponent) = SlotCardActors.createEntity(slotnum, playerId, slotState, game, resources, source)
      val target = getCoord(slotPanels(playerId).slots(slotnum).group)
      val move = new MoveToAction()

      move.setPosition(target.x + 15, target.y + 15)
      move setDuration 0.5f
      slotComponent.group addAction move
      val old = resources.slotSystem.findEntity(slotnum, playerId)
      resources.engine addEntity entity
      (slotComponent, old)
    }
    waitAction(slotComponent.group)
    // in case we summon over an already existing entity
    old foreach { x =>
      resources.beforeProcess invoke {
        resources.engine removeEntity x._1
      }
    }
    persistUpdater()
    controller.refresh(silent = true)
  }

  def die(num: Int, playerId: PlayerId) : Unit = {
    resources.slotSystem
      .findEntity(num, playerId)
      .foreach { case (entity, slot) =>
        waitAction(slot.fade())
        resources.beforeProcess invoke {
          resources.engine removeEntity entity
        }
      }
  }

  def refresh(silent: Boolean) = {
    persistUpdater()
    controller refresh silent
    updater.ended foreach endGame
  }

  def spellPlayed(c: Command) {
    notifySpell(c, sp.houses getHouseById c.card.houseId)
    (spellCast cast c) foreach (future => Await.result(future, Duration.Inf))
  }

  def triggerAbility(o: AnyRef) {

  }

  private def notifySpell(c : Command, house: House) : Unit = {
    val path = CardActors.getPath(c.card, house)
    val sprite = resources.atlas createSprite path
    val image = new Image(sprite)
    image.setPosition(50, 500)
    board.panel addAction new SpellPlayed(board.panel, image)
  }
}


class SpellPlayed(group : Group, image : Image) extends TemporalAction {
  setDuration(1f)

  protected override def begin() {
    group.addActor(image)
  }

  protected def update(percent: Float) = {
  }

  protected override def end(): Unit = {
    group.removeActor(image)
  }
}
