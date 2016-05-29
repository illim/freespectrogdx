package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.Group

import scala.language.dynamics
import com.badlogic.gdx.scenes.scene2d.ui.{HorizontalGroup, TextButton}
import com.mygdx.game.ScreenResources

import collection.mutable

class ButtonPanel(
  resources : ScreenResources,
  val panel : Group = new HorizontalGroup()) extends Dynamic {

  private val buttons = mutable.Map.empty[String, TextButton]

  def getButton(name : String) = {
    buttons.getOrElseUpdate(name, {
      val btn = new TextButton(name, resources.skin)
      panel addActor btn
      btn
    })
  }

  def selectDynamic(field : String) : TextButton = {
    getButton(field.substring(0, field.indexOf("Button")))
  }

  def clearListeners() : Unit = {
    buttons.foreach(_._2.clearListeners())
  }
}
