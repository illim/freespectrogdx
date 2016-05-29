package com.mygdx.game.gui

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.scenes.scene2d.ui.{List => _, _}
import com.badlogic.gdx.utils.Scaling
import com.mygdx.game.{onClick, ScreenResources}
import priv.sp._

object Dialogs {
  def center(dialog : Dialog) : Unit = {

    dialog.setPosition(
      (Gdx.graphics.getWidth - dialog.getWidth) / 2,
      (Gdx.graphics.getHeight - dialog.getHeight) / 2)
  }


  def createBtn(name : String, table : Table)(f : => Unit) = {
    val btn = new TextButton(name, table.getSkin)
    btn.addListener(onClick{
      f
    })
    btn
  }
}

class GameSettings(resources : GameResources, screenResources : ScreenResources) extends Dialog("game settings", screenResources.skin) { self =>
  import Dialogs.createBtn
  
  class PlayerChoice(id: PlayerId) {
    private val table = new Table

    val specials = resources.sp.houses.special
    var n = 0
    val choiceCheckBoxes : Map[String, CheckBox] = specials.map(_.name)
      .map { choice =>
        val checkbox = new CheckBox(choice, screenResources.skin)
        table add checkbox
        n += 1
        if (n % 2 == 0) table.row()
        (choice, checkbox)
      }.toMap
    table.row()

    val group = column(com.mygdx.game.gui.row(
      createBtn("all", self) { choiceCheckBoxes foreach (_._2.setChecked(true)) },
      createBtn("clear", self) { choiceCheckBoxes foreach (_._2.setChecked(false)) },
      createBtn("sinist", self) { select(resources.sp.houses.sinist) },
      createBtn("others", self) { select(resources.sp.houses.others) },
      createBtn("bs", self) { select(resources.sp.houses.bs) }),
      table)

    resources.playerChoices(id) foreach (h => choiceCheckBoxes(h.name).setChecked(true))
    updateResources()

    def updateResources() : Unit = {
      val selecteds = choiceCheckBoxes.collect { case (choice, checkbox) if checkbox.isChecked => choice }.toSet
      resources.playerChoices = resources.playerChoices.updated(id, specials.filter(x ⇒ selecteds.contains(x.name)))
    }

    def select(houses: List[House]) : Unit = {
      houses.foreach { h ⇒
        choiceCheckBoxes(h.name).setChecked(true)
      }
    }

  }

  screenResources.renderSystem setProcessing false  // HACK to avoid entities on dialog

  val choices = playerIds map { id ⇒ new PlayerChoice(id) }
  val pane = new SplitPane(choices(owner).group, choices(opponent).group, false, screenResources.skin)

  getContentTable.add(pane)

  button("ok", 'ok)
  button("cancel", 'cancel)
  pack()

  Dialogs.center(this)

  protected override def result (obj: AnyRef) : Unit = {
    obj match {
      case 'ok =>
        choices.foreach(_.updateResources())
      case _ =>
    }
    screenResources.renderSystem setProcessing true
  }

}

class HouseDescription (house : House, gameState : => GameState, playerId : PlayerId, resources : ScreenResources)
  extends Dialog(house.name, resources.skin) {

  val table = new Table()
  val pane = new ScrollPane(table)

  if (house.description.nonEmpty){
    val label = new Label(house.description, resources.skin2)
    label.setWrap(true)
    table.add(label).colspan(4).width(750)
    table.row()
  }

  var n = 0
  house.allCards foreach { card =>
    val path = CardActors.getPath(card, house)
    val image = new Image(resources.atlas createSprite path)
    val descriptionPanel = new DescriptionPanel(resources, descWidth = 300)

    descriptionPanel update Some(Description.cardToDesc(gameState, playerId, card))
    table.add(image).height(image.getHeight).center()
    table.add(descriptionPanel.panel).growY().top().left().pad(10)
    n += 1
    if (n % 2 == 0) {
      table.row()
    }
  }

  getContentTable add pane
  button("ok", 'ok)
  setResizable(true)
  pack()
  setWidth(math.min(getWidth, 800))
  setHeight(math.min(getHeight, 750))
  Dialogs.center(this)
  resources.renderSystem setProcessing false

  protected override def result (obj: AnyRef) : Unit = {
    resources.renderSystem setProcessing true
  }
}
