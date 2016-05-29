package com.mygdx.game.gui

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.scenes.scene2d.{Actor, Group}
import com.badlogic.gdx.scenes.scene2d.ui.Image
import com.mygdx.game.ScreenResources
import priv.sp.PlayerState
import priv.sp.house.BoundSouls


class SlotButton(val num: Int,
                 playerState: ⇒ PlayerState,
                 resources : ScreenResources)  {

  val group = new Group()

  val slotRegion = resources.atlas findRegion "combat/slot"
  group.setSize(slotRegion.getRegionWidth, slotRegion.getRegionHeight)
  group.setBounds(0, 0, slotRegion.getRegionWidth, slotRegion.getRegionHeight)

  val slotImage = new Image(slotRegion)

  group addActor slotImage
  group addActor new EnabledActor

  var enabled = false

  val boundSoul = new Image(resources.atlas findRegion "combat/soul")
  boundSoul.setPosition(16, 22)
  group addActor boundSoul

  refresh()

  def refresh(): Unit = {
    group.addAction(
      BasicAction{
        slotImage setVisible isExisting
        boundSoul setVisible hasBoundSoul
      })
  }

  def isExisting = playerState.slotList contains num
  def hasBoundSoul = {
    playerState.data match {
      case BoundSouls(souls) => souls contains num
      case _ => false
    }
  }

  class EnabledActor extends Actor with Initializable {
    var groupCoord = new Vector2

    override def draw(batch: Batch, parentAlpha: Float) = {
      if (enabled) {
        init(batch)
        batch.end()
        batch.begin()
        import resources.shapes
        shapes begin ShapeType.Line

        shapes setProjectionMatrix resources.stage.getCamera.combined.cpy().translate(groupCoord.x, groupCoord.y, 0)
        shapes setColor Color.BLUE
        shapes.rect(12, 18, 90, 100)
        shapes.end()
        batch.setShader(null)
        batch.end()
        batch.begin()
      }
    }

    override def initialize(batch : Batch): Unit = {
      groupCoord = getCoord(group)
    }
  }

}
