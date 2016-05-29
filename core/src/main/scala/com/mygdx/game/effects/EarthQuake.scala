package com.mygdx.game.effects

import com.badlogic.ashley.core.Entity
import com.mygdx.game.ScreenResources
import com.mygdx.game.component.{Scriptable, ScriptComponent, TimedComponent}
import com.mygdx.game.gui.SlotPanel
import priv.sp.PlayerId

/**
object EarthQuake {
  import scala.concurrent.ExecutionContext.Implicits.global

  def apply(resources : ScreenResources, slotPanel : SlotPanel) = {
    val entity = new Entity()
    val timedComponent = new TimedComponent(1f)
    val shader = resources.effectResources.ripple
    entity add timedComponent

    val script = new ScriptComponent(
      new Scriptable {
        def update(delta: Float): Unit = {
          val animationCursor = (timedComponent.time / timedComponent.duration) * 100
          shader.program.setUniformf(shader.cursor, animationCursor)
        }
      })

    slotPanel.slotPanel.set
    timedComponent.prom.future.onComplete { _ =>
      println("remove shaders")

    }
    (entity, timedComponent.prom)
  }
}*/
