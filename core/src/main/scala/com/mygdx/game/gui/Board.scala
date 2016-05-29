package com.mygdx.game.gui

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.Texture
import com.badlogic.gdx.scenes.scene2d.Group
import com.badlogic.gdx.scenes.scene2d.ui.{Image, VerticalGroup}
import com.badlogic.gdx.utils.Align
import com.mygdx.game.ScreenResources
import priv.sp._

import scala.util.Random

class Board(
  val playerId: PlayerId,
  val slotPanels: Seq[SlotPanel],
  val cardPanels : Seq[CardPanel],
  val descriptionPanel : DescriptionPanel,
  val historyPanel : DescriptionPanel,
  userMenu : ButtonPanel,
  resources: ScreenResources) {

  val rightPane = new VerticalGroup()

  rightPane align Align.right

  val otherCardPanel = cardPanels(other(playerId)).panel
  val cardPanel = cardPanels(playerId).panel

  rightPane addActor otherCardPanel
  rightPane addActor column(
      slotPanels(other(playerId)).panel,
      slotPanels(playerId).panel)
  rightPane addActor cardPanel
  rightPane.pack()
  val panel = new Group
  panel addActor rightPane
  panel addActor userMenu.panel
  panel addActor descriptionPanel.panel
  panel addActor historyPanel.panel

  resources.configure(descriptionPanel.panel, "description")
  resources.configure(historyPanel.panel, "history")
  resources.configure(userMenu.panel, "userMenu")
  rightPane.setX(50)

  def refresh(silent : Boolean) = {
    slotPanels foreach (_.refresh())
    cardPanels foreach (_.refresh(silent))
  }

}

class Background(resources :ScreenResources) {
  val background = new Group

  def setBackground(house  : House) = {
    val backgrounds = for {
      filename  <- exts.map(ext => house.name.toLowerCase + ext)
      file = Gdx.files.internal("backgrounds/" + filename)
      if file.exists()
    } yield file

    background.clear()
    val file = (Random shuffle backgrounds.toList).headOption getOrElse Gdx.files.internal("backgrounds/default.jpg")
    val texture = new Texture(file)
    val image = new Image(texture)
    image setY (800 - image.getHeight)
    background addActor image
  }

  def exts = for {
    n <- List("", "2", "3")
    ext <- List(".jpg", ".png")
  } yield n + ext
}

import priv.util.TVar

class CommandRecorder(val game : GameSession, board : Board) {
  private var value = Option.empty[Command]
  var cont = Option.empty[TVar[Option[Command]]]

  def setCommand(command: Command) {
    board.slotPanels foreach(_.disable())
    value = Some(command)
    nextStep()
  }

  def startWith(c: TVar[Option[Command]])(f: ⇒ Unit) {
    value = None
    cont = Some(c)
    f
  }

  def addInput(x: SlotInput) = {
    value foreach { command ⇒
      setCommand(command.copy(input = Some(x)))
    }
  }

  def skip() {
    continue(None)
  }

  private def continue(c: Option[Command]) = {
    cont.foreach(_.set(c))
    cont = None
  }

  private def nextStep() {
    value foreach { command ⇒
      if (command.card.inputSpec.size == command.input.size) {
        continue(Some(command))
      } else {
        import game._
        import board._
        def addInputOrEnable(playerId: PlayerId, slots: Traversable[Int]) {
          if (slots.size == 1) {
            addInput(new SlotInput(slots.head))
          } else {
            slotPanels(playerId) setSlotEnabled slots
          }
        }

        command.card.inputSpec.get match {
          case SelectOwner(f) ⇒
            addInputOrEnable(myPlayerId, f(myPlayerId, state))
          case SelectOwnerSlot ⇒
            addInputOrEnable(myPlayerId, PlayerState.openSlots(state.players(myPlayerId)))
          case SelectOwnerCreature ⇒
            addInputOrEnable(myPlayerId, state.players(myPlayerId).slots.keys.toList)
          case SelectTarget(f) ⇒
            addInputOrEnable(otherPlayerId, f(otherPlayerId, state))
          case SelectTargetSlot ⇒
            addInputOrEnable(otherPlayerId, PlayerState.openSlots(state.players(otherPlayerId)))
          case SelectTargetCreature ⇒
            addInputOrEnable(otherPlayerId, state.players(otherPlayerId).slots.keys.toList)
        }
      }
    }
  }
}
