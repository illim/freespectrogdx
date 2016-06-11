package com.mygdx.game.net

import com.badlogic.gdx.scenes.scene2d.ui.TextField.TextFieldListener
import com.badlogic.gdx.scenes.scene2d.ui.{List => UiList, _}
import com.mygdx.game._
import com.mygdx.game.gui._

class NetPanel(
  screens : Screens, buttons : ButtonPanel) {

  import screens._
  import screenResources.{skin2 => skin}

  val name = new TextField(System.getProperty("user.name"), skin)
  val host = new TextField("172.99.78.51", skin)
  val port = new TextField("12345", skin)
  val logs = new TextArea("", skin)
  val chat = new TextField("" , skin)
  val nbRows = 20
  logs setPrefRows nbRows

  val playerList = new UiList[PlayerInfo](skin)

  val table = new Table
  table.add(name).colspan(2).left()
  table.row()
  table.add(row(host, port)).colspan(2).left()
  table.row()
  table.add(logs).fillX().expandX()
  table.add(playerList).width(100).top()
  table.row()
  table.add(chat).colspan(2).fillX().pad(5)
  table.row()
  table.add(new Label("(Request or accept a duel by sending: /duel playername)", skin))

  val panel = table
  table.pad(5).bottom().pack()

  buttons.ConnectButton addListener onClick {
    screenResources.clientOption foreach { client => client.release()  }
    try {
      val client = new NetClient(
        host.getText, port.getText.toInt, name.getText.trim,
        screens,
        logText, logDuelRequest, setPlayerList)
      screenResources.clientOption = Some(client)
      logText("Connected")
    } catch { case e : Exception =>
      logText(e.getMessage)
      screens.lastE = Some(e)
    }
  }

  chat setTextFieldListener new TextFieldListener {
    override def keyTyped(textField: TextField, c: Char) : Unit = {
      if (c == '\r' || c == '\n') {
        screenResources.clientOption foreach { client =>
          val text = chat.getText
          if (text.startsWith("/duel ")) {
            val name = text.replace("/duel ", "")

            logText("Requesting a duel to " + name + "...")
            playerList.getItems.toArray().find(_.name == name && client.user != name) match {
              case None => logText(name + " not found")
              case Some(p) =>
                client send Message(Header(MessageType.RequestDuel), Some(p.id.getBytes))
            }
          } else {
            client proxyMessage ChatMessage(client.user + ": " + text)
          }
          chat setText ""
        }
      }
    }
  }


  def setPlayerList(players : List[PlayerInfo]) = {
    playerList.setItems(players : _*)
  }

  def logText(s : String) = {
    logs.appendText(s + "\n")
  }

  def logDuelRequest(id : String) = {
    playerList.getItems.toArray().find(_.id == id) match {
      case None => logText("duel request from unknown id " + id)
      case Some(p) => logText(p.name + " invite you to a duel")
    }
  }
}
