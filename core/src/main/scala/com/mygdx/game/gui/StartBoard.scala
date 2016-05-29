package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.Group
import com.badlogic.gdx.scenes.scene2d.ui.VerticalGroup
import com.mygdx.game._
import com.mygdx.game.net.NetPanel

class StartBoard(screens : Screens) {

  val buttonPanel = new ButtonPanel(screens.screenResources, new VerticalGroup)
  val panel       = new Group
  val netPanel    = new NetPanel(screens)

  panel addActor row(buttonPanel.panel, netPanel.panel)
}

