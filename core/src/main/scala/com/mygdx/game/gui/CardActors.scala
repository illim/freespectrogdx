package com.mygdx.game.gui

import com.badlogic.ashley.core.Entity
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.math.Vector2
import com.badlogic.gdx.scenes.scene2d.actions.TemporalAction
import com.badlogic.gdx.scenes.scene2d.{Actor, Group}
import com.badlogic.gdx.scenes.scene2d.ui.{Image, Label}
import com.mygdx.game.ScreenResources
import com.mygdx.game.component.{VisualComponent, SlotComponent}
import com.typesafe.config.Config
import priv.sp._
import priv.sp.house.{SoulReaper, Limbo, Hird, MereMortal}

object CardActors {

  def getPath(card : Card, house : House) = {
    val houseFolder = if (card.houseIndex < 4) "base" else house.name.toLowerCase.replaceAll(" ", "")
    "cards/" + houseFolder + "/" + card.name.replace("'", "")
  }
}

class CardActors(val card: Card, house : House, val resources : ScreenResources) {
  val path = CardActors.getPath(card, house)
  val sprite = resources.atlas createSprite path

  if (sprite == null) throw new Exception("sprite not found " + path)

  val cardImage = new Image(sprite)
  if (card.isSpell) cardImage.setPosition(-1, -1)
  else              cardImage.setPosition(3, 14)

  val borderName = if (card.isSpell) "rakaSpell" else "raka"
  val borderTex = resources.atlas findRegion ("combat/"+borderName)

  val costLabel = new Label(card.cost.toString, resources.skin)
  costLabel setColor Color.BLUE
  val labels :List[Label] = card match {
    case spell: Spell ⇒
      resources.configure(costLabel, "spell.label.cost")
      List(costLabel)
    case creature: Creature ⇒
      resources.configure(costLabel, "card.label.cost")
      val attackLabel = new Label(creature.attack.base.map(_.toString) getOrElse "?", resources.skin)
      val lifeLabel = new Label(creature.life.toString, resources.skin)
      attackLabel setColor Color.RED
      resources.configure(attackLabel, "card.label.attack")
      lifeLabel setColor Color.OLIVE
      resources.configure(lifeLabel, "card.label.life")
      List(costLabel, attackLabel, lifeLabel)
  }


  val borderImage = new Image(borderTex)

  val actors = cardImage :: borderImage :: labels

  assert(sprite != null, "sprite not defined " + path)

  def updateZIndex(): Unit ={
    cardImage.setZIndex(0)
    borderImage.setZIndex(2)
    labels.foreach(_.setZIndex(3))
  }

}


object SlotCardActors {

  def createActors(slotState : SlotState, game : GameSession, resources : ScreenResources) = {
    val group = new Group
    val cardGroup = new Group
    cardGroup.getColor.a = 1f
    assert(slotState.card.houseId != 0, "houseId not set for " + slotState.card)
    val cardActors = new SlotCardActors(
      cardGroup,
      slotState.card,
      new CardActors(slotState.card, game.sp.houses.getHouseById(slotState.card.houseId), resources),
      game)
    cardActors.update(slotState)
    cardActors.updateZIndex()
    cardGroup.getColor.a = if (slotState has CardSpec.pausedFlag) 0.5f else 1f
    group addActor cardGroup
    (group, cardActors)
  }

  def createEntity(slotnum : Int, playerId : PlayerId, slotState : SlotState, game : GameSession, resources : ScreenResources, source : Vector2) = {
    val (group, slotCardActors) = SlotCardActors.createActors(slotState, game, resources)
    val entity = new Entity
    val slotComponent = new SlotComponent(slotnum, playerId, slotCardActors, group, game, resources)

    group.setPosition(source.x, source.y)
    entity add slotComponent
    entity add new VisualComponent(slotComponent)
    (entity, slotComponent)
  }
}

class SlotCardActors(val cardGroup : Group, card : Creature, cardActors : CardActors, game : GameSession) {
  val lifeLabel = cardActors.labels(2)
  val costLabel = cardActors.labels(1)
  val lifeBarTex = cardActors.resources.atlas findRegion "combat/lifebar"
  var life = card.life
  var maxLife = card.life
  val lifeDamagable = new DamagableInt(life, lifeLabel, cardActors.resources)
  val lifeBar = new Image(lifeBarTex) {
    setZIndex(4)
    setY(cardActors.borderTex.getRegionHeight - lifeBarTex.getRegionHeight)
    override def act(delta : Float): Unit = {
      setWidth(66f * life / math.max(life, maxLife))
    }
  }
  var decorators = List.empty[Actor]

  def pauseColor = new Color(0.4f, 0.4f, 0.4f, 1f)

  def update(slotState : SlotState) : Unit = {
    lifeLabel setText slotState.life.toString
    costLabel setText slotState.attack.toString
    life = slotState.life
    maxLife = slotState.maxLife
    lifeDamagable.refresh()
    decorators = decorateStatus(slotState).toList
    cardGroup.clearChildren()
    actors foreach cardGroup.addActor
    updateZIndex()

    // FIXME BS
    if (slotState has CardSpec.pausedFlag) {
      actors foreach { a =>
        if (a.getColor == Color.WHITE) {
          a setColor pauseColor
        }
      }
    } else {
      actors foreach { a =>
        if (a.getColor == pauseColor) {
          a setColor Color.WHITE
        }
      }
    }
  }

  def actors = cardActors.actors ++ (lifeBar :: decorators)

  def decorateStatus(s: SlotState) = {
    def imageOf(name : String) = {
      val image = new Image(cardActors.resources.atlas findRegion ("combat/" + name))
      image.setPosition(2, 15)
      image
    }
    def cornerImageOf(name : String) = {
      val image = imageOf(name)
      image.setPosition(50, 60)
      image
    }

    if      ( s has CardSpec.blockedFlag )     Some(imageOf("crystalize"))
    else if ( s has CardSpec.stunFlag )        Some(imageOf("sand"))
    else if ( s.card.isInstanceOf[MereMortal]) Some(imageOf("mortal"))
    else if ( s has CardSpec.invincibleFlag )  Some(cornerImageOf("shield"))
    else if ( s has CardSpec.cursedFlag )      Some(cornerImageOf("death"))
    else if ( s.data == Limbo.LimboState)      Some(imageOf("limbo"))
    else if ( s.data == SoulReaper.Eternal)    Some(cornerImageOf("eternalrage"))
    else if ( s.card.houseId == game.sp.houses.moutainKing.MoutainKing.houseId && s.data == Hird) Some(cornerImageOf("hird"))
    else None
  }

  def updateZIndex(): Unit ={
    decorators.foreach(_.setZIndex(1))
    cardActors.updateZIndex()
    lifeBar.setZIndex(4)
  }
}


class Focus extends TemporalAction {
  var start: Float = 0f
  val amplitude = 0.05
  setDuration(0.5f)

  protected override def begin() {
    start = target.getScaleX
  }

  protected def update(percent: Float) = {
    val delta = amplitude * math.sin(percent * math.Pi)
    target.setScale(1 + delta.toFloat)
  }

  protected override def end(): Unit = {
    target.setScale(start)
  }
}

class Run(direction: Int, config : Config) extends TemporalAction {
  var start: Float = 0f
  val amplitude = config.getDouble("amplitude").toFloat
  val duration = config.getDouble("duration").toFloat
  setDuration(duration)

  protected override def begin() {
    start = target.getY()
  }

  protected def update(percent: Float) = {
    target.setY(start + (amplitude * direction * (0.5f - math.abs(0.5f - percent))))
  }

  protected override def end(): Unit = {
    target.setY(start)
  }
}