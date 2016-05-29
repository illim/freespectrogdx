package com.mygdx.game.component

import com.badlogic.ashley.core._
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.scenes.scene2d.actions.MoveToAction
import com.badlogic.gdx.scenes.scene2d.ui.Label
import com.badlogic.gdx.scenes.scene2d.ui.Label.LabelStyle

object DamageEntity {

  def apply(d : DamageComponent) : Entity = {
    val entity = new Entity
    entity add new TimedComponent(1f)
    entity add new ScriptComponent(d)
    entity add new VisualComponent(d)
    entity
  }
}

class DamageComponent(val d : Int, pos : Vector2, style : LabelStyle, direction : Int = 1)
  extends Drawable
  with Scriptable {

  val damageLabel = new Label(if (d>0) "+"+d else d.toString, style)
  damageLabel.setColor(if (d > 0) Color.GREEN else Color.RED)
  damageLabel.setPosition(pos.x, pos.y)

  val moveAction = new MoveToAction()
  moveAction.setPosition(pos.x, pos.y + 20 * direction)
  moveAction setDuration 1f
  damageLabel addAction moveAction

  def draw(batch : Batch) : Unit = {
    damageLabel.draw(batch, 1)
  }

  def update(delta : Float) : Unit = {
    damageLabel act delta
  }
}


