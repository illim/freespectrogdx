package com.mygdx.game.net

import com.badlogic.gdx.scenes.scene2d.ui.{List => _, _}
import com.mygdx.game._
import com.mygdx.game.gui.ButtonPanel

class NetPanel(
  screens : Screens) {

  import screens._
  import screenResources.skin

  val name = new TextField(System.getProperty("user.name"), skin)
  val host = new TextField("localhost", skin)
  val port = new TextField("12345", skin)
  val logs = new TextArea("", skin)
  val nbRows = 30
  logs setPrefRows nbRows
  val playerList = new TextArea("", skin)
  playerList setPrefRows nbRows

  val buttons = new ButtonPanel(screenResources)
  val panel = gui.column(
    name,
    gui.row(host, port),
    buttons.panel,
    gui.row(logs, playerList))

  buttons.connectButton addListener onClick {
    screenResources.clientOption foreach { client => client.release()  }
    try {
      val client = new NetClient(
        host.getText, port.getText.toInt, name.getText,
        screens,
        logText, setPlayerList)
      screenResources.clientOption = Some(client)
      logText("Connected")
    } catch { case e : Exception =>
      logText(e.getMessage)
      screens.lastE = Some(e)
    }
  }

  buttons.searchButton addListener onClick {
    screenResources.clientOption foreach { client =>
      logText("Searching...")
      client send Message(Header(MessageType.RequestDuel))
    }
  }

  def setPlayerList(players : List[String]) = playerList setText players.mkString("\n")

  def logText(s : String) = {
    logs.appendText(s + "\n")
  }
}
