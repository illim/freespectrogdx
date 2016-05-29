package com.mygdx.game.gui


import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.Group
import com.badlogic.gdx.scenes.scene2d.ui.{VerticalGroup, Label}
import com.badlogic.gdx.utils.Align
import com.mygdx.game.ScreenResources
import com.mygdx.game.component.{DamageEntity, DamageComponent}
import priv.sp.House
import priv.sp.house.SoulReaperData

class LifeLabel(name: String, getLife : => Int, resources : ScreenResources) {
  val panel = new VerticalGroup()
  val label         = new Label(name + " ", resources.skin)
  val lifeDamagable = DamagableInt(getLife, resources)
  val phaseLabel    = new Label("", resources.skin)
  val lifeRow       = row(label, lifeDamagable.label)
  lifeRow.align(Align.bottom)

  lifeRow    align    Align.bottomLeft
  label      setColor Color.WHITE
  phaseLabel setColor Color.GRAY
  panel      addActor lifeRow

  panel.pack()

  val setPhase : Option[String] => Unit = {
    case None => panel removeActor phaseLabel
    case Some(s) =>
      phaseLabel setText s
      panel addActor phaseLabel
  }
}


class HouseLabel(getMana : => Int, val house: House, resources : ScreenResources, flip: Boolean = false) {
  val label = new Label(house.name + " ", resources.skin)
  val direction = if (flip) -1 else 1
  val manaDamageable = DamagableInt(getMana, resources, if (flip) -1 else 1)
  val panel = new Group()
  panel.setHeight(10)
  val actor = row(label, manaDamageable.label)
  resources.configure(actor, "house.label")
  panel.addActor(actor)
}

object DamagableInt {

  def apply(getValue: ⇒ Int, resources : ScreenResources, direction : Int = 1) = {
    val label = new Label("0", resources.skin)
    val damagable = new DamagableInt(getValue, label, resources, direction)
    label.setText(damagable.current.toString)
    damagable
  }
}

class DamagableInt(getValue: ⇒ Int, val label : Label, resources : ScreenResources, direction : Int = 1)  {
  var current = getValue

  def refresh(silent: Boolean = false) {
    val old = current
    current = getValue
    label.addAction(new UpdateAction[Label](_.setText(current.toString)))
    val d = current - old
    if (d != 0 && !silent) {
      resources.engine addEntity DamageEntity(new DamageComponent(d, getCoord(label), label.getStyle, direction))
    }
  }
}

class DataPanel(getData : => Any, resources : ScreenResources) {
  val label = new Label("", resources.skin)
  resources.configure(label, "dataPanel")
  def refresh() = {
    getData match {
      case SoulReaperData(x) => label.setText(x + " souls")
      case _ =>
    }
  }
}