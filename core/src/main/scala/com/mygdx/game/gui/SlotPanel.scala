package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.badlogic.gdx.scenes.scene2d.{InputEvent, Group}
import com.badlogic.gdx.scenes.scene2d.ui.HorizontalGroup
import com.mygdx.game._
import priv.sp._

class SlotPanel(playerId: PlayerId, val game: GameSession, descriptionPanel : DescriptionPanel, resources : ScreenResources) {
  val lifeLabel = new LifeLabel(game.names(playerId), game.state.players(playerId).life, resources)
  val dataPanel = new DataPanel(game.state.players(playerId).data, resources)
  val slots     = baseSlotRange.map { num ⇒ new SlotButton(num, game.state.players(playerId), resources) }.toList
  val panel     = new Group

  panel addActor lifeLabel.panel
  lifeLabel.panel setY 50

  panel addActor dataPanel.label

  val slotPanel = new HorizontalGroup()
  slots.map(_.group) foreach slotPanel.addActor
  slotPanel.pack()

  slotPanel.setX(100)
  panel.addActor(slotPanel)
  panel.setHeight(slotPanel.getHeight)
  panel.setWidth(slotPanel.getWidth + 100)


  def init(commandRecorder: CommandRecorder) = {
    def listenEvent(slotButton: SlotButton) {
      slotButton.group.addListener(new ClickListener with HoverToDesc {
        def descPanel = descriptionPanel
        def getDescription = {
          val playerState = game.state.players(playerId)
          playerState.slots.get(slotButton.num).map(s => Description.cardToDesc(game.state, playerId, s.card))
        }
        override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
          if (slotButton.enabled) {
            commandRecorder addInput new SlotInput(slotButton.num)
          }
        }
      })
    }
    slots foreach listenEvent
  }

  def setSlotEnabled(s: Traversable[Int]) {
    val nums = s.toSet
    slots foreach { slot ⇒ slot.enabled = nums contains slot.num }
  }
  def disable() { slots foreach (_.enabled = false) }
  def refresh() {
    lifeLabel.lifeDamagable.refresh()
    slots foreach (_.refresh())
    dataPanel.refresh()
  }
}
