package com.mygdx.game.gui

import com.badlogic.gdx.graphics.GL20
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.scenes.scene2d.Actor
import com.mygdx.game.ScreenResources

class SelectedEffectActor(resources : ScreenResources) extends Actor {
  var time = 0f
  val seletectedShader = resources.effectResources.selected
  var groupCoord = new Vector2
  var selected = false

  override def draw(batch: Batch, parentAlpha: Float) = {
    if (selected) {
      import seletectedShader._
      val deltax = time * 10
      val animationCursor = deltax % animLength

      program.begin()
      program.setUniformf(cursor, animationCursor)
      program.setUniformf(size, seletectedShader.sizeConfig)
      program.setUniformMatrix("u_projTrans",
        resources.stage.getCamera.combined.cpy()
          .translate(groupCoord.x + seletectedShader.offsetx, groupCoord.y + seletectedShader.offsety, 0))
      mesh.render(program, GL20.GL_TRIANGLE_STRIP)
      program.end()
      batch.setShader(null) // why the fuck
    }
  }

  override def act(delta : Float): Unit = {
    if (selected) {
      super.act(delta)
      time += delta
    }
  }
}

class HoveredActor(resources : ScreenResources) extends Actor {
  val effect = resources.effectResources.particles.get("smoke")
  effect.reset()
  effect.start()

  var enabled = false

  override def draw(batch: Batch, parentAlpha: Float) = {
    if (enabled) {
      effect.draw(batch)
    }
  }

  override def act(delta : Float): Unit = {
    if (enabled) {
      effect update delta
    }
  }
}