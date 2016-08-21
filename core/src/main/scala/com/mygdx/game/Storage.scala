package com.mygdx.game

import java.util.Properties
import com.badlogic.gdx.Gdx
import priv.util.Utils

object Storage {
  val USER_NAME = "user.name"
  val CARD_THEME = "card.theme"
  val SERVER = "server.host"
  val SERVER_PORT = "server.port"
  val CLASS_CHOICE = List("class.choice", "ai.class.choice")
}

import Storage._

class Storage {
  val userPrefsPath = Gdx.files external ".freespectro/user.prefs"
  val userPrefs     = new Properties()
  val checksum      = Utils.getChecksum() // store this? to check if changed to redownload assets (or try to check assets?)
  println("checksum " + checksum)

  var userName       = Option.empty[String]
  var server         = "172.99.78.51"
  var serverPort     = "12345"
  var cardTheme      = "original"
  var classesChoices = List(List.empty[String], List.empty[String])

  if (userPrefsPath.exists()) {
    userPrefs.load(userPrefsPath.reader())
    initVars()
  }

  def persist(m : Map[String, String]) : Unit = {
    m foreach { case (k, v) => userPrefs.setProperty(k, v) }
    userPrefs.store(userPrefsPath.writer(false), "user preferences")
    initVars()
  }

  private def initVars() : Unit = {
    Option(userPrefs getProperty USER_NAME) foreach (x => userName = Some(x))
    Option(userPrefs getProperty SERVER) foreach (x => server = x)
    Option(userPrefs getProperty SERVER_PORT) foreach (x => serverPort = x)
    Option(userPrefs getProperty CARD_THEME) foreach (x => cardTheme = x)
    CLASS_CHOICE.zipWithIndex foreach { case (key, idx) =>
      Option(userPrefs getProperty key) foreach (x => classesChoices = classesChoices.updated(idx, x.split(",").toList))
    }
  }

}
