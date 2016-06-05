package com.mygdx.game

import java.lang.management.ManagementFactory

import com.badlogic.gdx.{Application, Gdx, Game}

class MyGdxGame extends Game {
  println("app :" + ManagementFactory.getRuntimeMXBean().getName())

  lazy val screens = new Screens(this)

  override def create() {
    Gdx.app.setLogLevel(Application.LOG_DEBUG)
    screens.startScreen.select()
  }
}

