package com.mygdx.game.gui

import java.util.concurrent.atomic.AtomicBoolean

import com.badlogic.gdx.graphics.g2d.Batch

trait Initializable {
  val initialized = new AtomicBoolean()
  def init(batch : Batch): Unit = {
    if (initialized.compareAndSet(false, true)) {
      initialize(batch)
    }
  }

  def initialize(batch : Batch): Unit = {}
}
