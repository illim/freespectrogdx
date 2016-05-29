package com.mygdx

import com.badlogic.gdx.scenes.scene2d.InputEvent
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener

package object game {

  def onClick(f : => Unit) = new ClickListener() {
    override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
      f
    }
  }
}
