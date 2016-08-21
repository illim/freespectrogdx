package com.mygdx.game.net

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.scenes.scene2d.InputEvent
import com.badlogic.gdx.scenes.scene2d.ui.TextField.TextFieldListener
import com.badlogic.gdx.scenes.scene2d.ui.{List => UiList, _}
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.mygdx.game._
import com.mygdx.game.effects.SoundEntity
import com.mygdx.game.gui._
import priv.util.GuiUtils._
import priv.sp.I18n

class NetPanel(
  screens : Screens, buttons : ButtonPanel) {

  import screens._
  import screenResources.{skin2 => skin}

  val name = new TextField(screens.storage.userName getOrElse System.getProperty("user.name"), skin)
  val host = new TextField(screenResources.storage.server, skin)
  val port = new TextField(screenResources.storage.serverPort, skin)
  val logs = new TextArea("", screenResources.commonSkin)
  val chat = new TextField("" , skin)
  val nbRows = 20
  logs setPrefRows nbRows
  logs setDisabled true

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
  table.add(new Label(I18n("duelrequest.tooltip"), skin))

  val panel = table
  table.pad(5).bottom().pack()

  playerList.addListener(new ClickListener(){ self =>
    override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
      if (self.getTapCount == 2) {
        val name = playerList.getSelected
        if (name!= null) {
          requestDuel(name.name)
        }
      }
    }
  })

  buttons.getButton(I18n("button.connect")) addListener onClick {
    screenResources.clientOption foreach { client => client.release()  }
    try {
      val client = new NetClient(
        host.getText, port.getText.toInt, name.getText.trim,
        screens,
        logText, logDuelRequest, setPlayerList)
      screenResources.clientOption = Some(client)
      screenResources.storage persist Map(
        Storage.USER_NAME -> client.user,
        Storage.SERVER -> host.getText,
        Storage.SERVER_PORT -> port.getText)
      logText("Connected")
    } catch { case e : Exception =>
      logText(e.getMessage)
      screens.lastE = Some(e)
    }
  }

  chat setTextFieldListener new TextFieldListener {
    override def keyTyped(textField: TextField, c: Char) : Unit = {
      if (c == '\r' || c == '\n') {
        screenResources.clientOption match {
          case None => logText("Not connected")
          case Some(client) =>
            val text = chat.getText
            if (text.startsWith("/duel ")) {
              val name = text.replace("/duel ", "")
              requestDuel(name)
            } else {
              client proxyMessage ChatMessage(client.user + ": " + text)
            }
            chat setText ""
        }
      }
    }
  }

  def requestDuel(name : String) = {
    logText("Requesting a duel to " + name + "...")
    screenResources.clientOption match {
      case None => logText("Not connected")
      case Some(client) =>
        playerList.getItems.toArray().find(_.name == name && client.user != name) match {
          case None => logText(name + " not found")
          case Some(p) =>
            client send Message(Header(MessageType.RequestDuel), Some(p.id.getBytes))
        }
    }
  }

  def setPlayerList(players : List[PlayerInfo]) = {
    playerList.setItems(players : _*)
  }

  def logText(s : String) = {
    logs.appendText(s + "\n")
    playSound("sounds/chat.mp3")
  }

  def logDuelRequest(id : String) = {
    playerList.getItems.toArray().find(_.id == id) match {
      case None => logText("duel request from unknown id " + id)
      case Some(p) => logText(p.name + " invite you to a duel")
    }
    playSound("sounds/duel-request.mp3")
  }

  def isInLobby() = screens.game.getScreen.isInstanceOf[LobbyScreen]
  def playSound(name : String) = {
    if (isInLobby()) {
      val sound = Gdx.audio.newSound(Gdx.files.internal(name))
      screenResources.engine addEntity SoundEntity(sound, 3, screenResources)
    }
  }
}
