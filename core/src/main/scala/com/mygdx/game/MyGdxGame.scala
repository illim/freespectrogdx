package com.mygdx.game

import com.badlogic.gdx.Game

class MyGdxGame extends Game {

  lazy val screens = new Screens(this)

  override def create() {
    screens.startScreen.select()
  }
}

