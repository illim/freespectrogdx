package com.mygdx.game.gui

import com.badlogic.gdx.scenes.scene2d.Group
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


  val group = new Group
  group.setSize(90, 102)


  var cardActorsOption = getDesc.map(d ⇒ CardButtonActors(d, getHouseState, new CardActors(d.card, houseDesc.house, resources)))

  var visible = false
  var enabled = false
  def isActive = cardActorsOption.exists(_.isActive && enabled)

  refresh()

  def refresh() = {
    group.clear()
    if (visible) {
      cardActorsOption foreach { cardActors =>
        cardActors.cardActors.actors foreach group.addActor
      }
    }
  }

  /**import game.sp

  var holder = getDesc.map(d ⇒ new CardHolder(d, getHouseState))
  val size = holder.map(_.borderTex).getOrElse(sp.baseTextures.borderTex).size
  private var hovered = false
  private val grey = sp.shaders get "grey"
  private val hoverGlow = sp.baseShaders.hoverGlow
  private val selectedGlow = sp.baseShaders.selectedGlow("selcard", 200)
  var selected = false

  def card = holder.map(_.desc.card)
  def isActive = holder.exists(_.isActive)

  class CardHolder(val desc: CardDesc, houseState: HouseState) {
    val cardTex = sp.textures.get("Images/Cards/" + desc.card.image)
    val borderTex = sp.baseTextures getBorder desc.card
    def isActive = (desc isAvailable houseState) && enabled
  }

  def refresh() {
    holder = getDesc.map(d ⇒ new CardHolder(d, getHouseState))
  }*/

  /**
  def render() {
    if (visible) {
      holder.map { h ⇒
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        val isActive = h.isActive
        glColor4f(1, 1, 1, 1)

        if (!isActive) {
          grey.begin()
        } else if (isActive && hovered && !selected) {
          glPushMatrix()
          glTranslatef(-5, -5, 0)
          hoverGlow.used {
            val deltax = getDelta() / 100f
            val animLength = 50
            val animationCursor = deltax.intValue % animLength
            glUniform1i(hoverGlow.cursor, animationCursor)
            tex.draw(sp.baseTextures.cardGlow)
          }
          glPopMatrix()
        } else if (selected) {
          val o = Coord2i(size.x / 2 - 100, size.y / 2 - 100)
          glDisable(GL_TEXTURE_2D)
          selectedGlow.used {
            val deltax = getDelta() / 100f
            val animLength = 62
            val animationCursor = deltax % animLength
            glUniform1f(selectedGlow.cursor, animationCursor)
            glUniform2f(selectedGlow.offset, o.x, o.y)
            glBegin(GL_POLYGON)
            glVertex2f(o.x, o.y)
            glVertex2f(o.x + 200, o.y)
            glVertex2f(o.x + 200, o.y + 200)
            glVertex2f(o.x, o.y + 200)
            glEnd()
            glEnable(GL_TEXTURE_2D)
          }
        }

        glPushMatrix()
        if (h.desc.card.isSpell) glTranslatef(-1, -1, 0) else glTranslatef(3, 8, 0)
        tex.draw(h.cardTex)
        glPopMatrix()

        tex.draw(h.borderTex)

        h.desc.card match {
          case spell: Spell ⇒
            Fonts.font.draw(72, 9, h.desc.cost, 'blue)
          case creature: Creature ⇒
            Fonts.font.draw(72, 1, h.desc.cost, 'blue)
            Fonts.font.draw(4, 80, creature.attack.base.map(_.toString) getOrElse "?", 'red)
            Fonts.font.draw(70, 80, creature.life, 'green)
        }
        if (!isActive) grey.end()
      }
    } else {
      if (holder.isDefined) {
        glDisable(GL_TEXTURE_2D)
        glColor4f(0.1f, 0.1f, 0.1f, 1)
        glBegin(GL_POLYGON)
        glVertex2f(0, 0)
        glVertex2f(85, 0)
        glVertex2f(85, 97)
        glVertex2f(0, 97)
        glEnd()
        glEnable(GL_TEXTURE_2D)
      }
    }
  }
  on {
    case MouseMoved(_) ⇒
      game.descriptionPanel.describedOption = holder.map(_.desc.card)
      hovered = true
    case MouseLeaved(_) ⇒
      game.descriptionPanel.describedOption = None
      hovered = false
  }*/

}

/**
 * case class TestButton(sp: SpWorld) extends GuiElem {
 * val size = Coord2i(200, 200)//sp.baseTextures.blank.coord
 * val selectedGlow = sp.baseShaders.selectedGlow("test", size.x)
 * def render() {
 * glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
 * glDisable(GL_TEXTURE_2D)
 * glColor4f(1, 1, 1, 1)
 * selectedGlow.used {
 * val deltax = getDelta() / 50f
 * val animLength = 50
 * val animationCursor = deltax % animLength
 * val o = Coord2i(0, 0)
 * glUniform1f(selectedGlow.cursor, animationCursor)
 * glUniform2f(selectedGlow.offset, 0, 0)
 * glBegin(GL_POLYGON)
 * glVertex2f(o.x, o.y)
 * glVertex2f(o.x + size.x,o.y)
 * glVertex2f(o.x + size.x,o.y + size.y)
 * glVertex2f(o.x, o.y + size.y)
 * glEnd()
 * glEnable(GL_TEXTURE_2D)
 * }
 * }
 * override def updateCoord(c : Coord2i){
 * super.updateCoord(c)
 * println("testcoord" + c)
 * }
 * }
 */
