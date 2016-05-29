package com.mygdx.game.gui

import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import com.badlogic.gdx.graphics.{Color, GL20}
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.scenes.scene2d.{Actor, Group}
import com.mygdx.game.ScreenResources
import priv.sp._

case class CardButtonActors(desc : CardDesc, houseState : HouseState, cardActors : CardActors) {
  cardActors.costLabel.setText(desc.cost.toString)
  def isActive = desc isAvailable houseState
}

class CardButton(getDesc: ⇒ Option[CardDesc],
                 getHouseState: ⇒ HouseState,
                 houseDesc : PlayerHouseDesc,
                 resources : ScreenResources)  {

  var cardActorsOption = getCardActors

  var visible  = false
  var enabled  = false
  var selected = false
  var hovered  = false
  def isActive = cardActorsOption.exists(_.isActive && enabled)
  def getCardOption = cardActorsOption.map(_.desc.card)

  val group = new Group {

    override def draw(batch: Batch, parentAlpha: Float) = {
      if (visible) {
        if (!isActive && visible) {
          batch.setShader(resources.effectResources.grey.program)
        }
        try {
          super.draw(batch, parentAlpha)
        } finally {
          if (batch.getShader != null) batch.setShader(null)
        }
      }
    }

  }

  val raka = resources.atlas findRegion "combat/raka"

  group.setSize(raka.getRegionWidth, raka.getRegionHeight)
  group.setBounds(0, 0, raka.getRegionWidth, raka.getRegionHeight)

  refresh()

  def refresh() = {
    group.addAction(
      BasicAction {
        group.clearChildren()
        if (visible) {
          cardActorsOption = getCardActors
          cardActorsOption foreach { cardActors =>
            group addActor grayableActor
            cardActors.cardActors.actors foreach group.addActor
          }
        }
      })
  }

  private def getCardActors = getDesc.map(d ⇒ CardButtonActors(d, getHouseState, new CardActors(d.card, houseDesc.house, resources)))

  private val grayableActor = new Actor with Initializable {
    var groupCoord = new Vector2

    override def draw(batch: Batch, parentAlpha: Float) = {
      if (!visible && cardActorsOption.isDefined) {
        import resources.shapes
        shapes.begin(ShapeType.Filled)
        shapes.setProjectionMatrix(resources.stage.getCamera.combined.cpy().translate(groupCoord.x, groupCoord.y, 0))
        shapes.setColor(Color.DARK_GRAY)
        shapes.rect(0, 0, 85 ,97)
        shapes.end()
        shapes.flush()
        batch.setShader(null)
      }
    }

    override def initialize(batch : Batch): Unit = {
      groupCoord = getCoord(group)
    }
  }

}
