package com.mygdx.game.gui

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.badlogic.gdx.scenes.scene2d.{Actor, InputEvent}
import com.badlogic.gdx.scenes.scene2d.ui.{Table, Container, VerticalGroup, Label}
import com.badlogic.gdx.utils.Align
import com.mygdx.game.ScreenResources
import priv.sp._

class DescriptionPanel(resources : ScreenResources,
                       color : Color = Color.WHITE,
                       descWidth : Float = 350) {
  val panel = new Table()
  val title = new Label("", resources.skin)
  title setFontScale 1.1f
  val subtitle = new Label("", resources.skin)
  val description = new Label("", resources.skin2)
  description setWrap true
  panel align Align.bottomLeft
  panel.add(title).left
  panel.add(subtitle).left
  panel.row()
  panel.add(description).left() width descWidth colspan 2
  List(title, subtitle, description) foreach (_.setColor(color))

  val update : Option[(String, String, String)] => Unit = {
    case None => panel setVisible false
    case Some((titleText, subtitleText, descriptionText)) =>
      title setText titleText
      subtitle setText subtitleText
      description setText descriptionText
      panel setVisible true
  }
}

object Description {
  def houseToDesc(h : House) = (h.name, "", h.description)
  def cardToDesc(gameState : GameState, playerId : PlayerId, card : Card) = {
    val subtitle = card match {
      case c: Creature ⇒ "Life : " + c.life + "  Attack : " + c.attack.base.getOrElse("X")
      case _ ⇒ ""
    }
    (card.name + " ("+card.cost+")", subtitle, card.description(gameState, playerId))
  }
}
trait HoverToDesc extends ClickListener {
  def descPanel : DescriptionPanel

  def getDescription : Option[(String, String, String)]

  override def enter(event: InputEvent, x: Float, y: Float, pointer : Int, fromActor : Actor) : Unit = {
    super.enter(event, x, y, pointer, fromActor)
    if (isOver) {
      descPanel.update(getDescription)
    }
  }
  override def exit(event: InputEvent, x: Float, y: Float, pointer : Int, fromActor : Actor) : Unit = {
    super.exit(event, x, y, pointer, fromActor)
    if (!isOver) {
      descPanel.update(None)
    }
  }
}
