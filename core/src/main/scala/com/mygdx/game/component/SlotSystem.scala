package com.mygdx.game.component

import com.badlogic.ashley.core.{Entity, ComponentMapper, Family, Component}
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.scenes.scene2d.actions.AlphaAction
import com.badlogic.gdx.scenes.scene2d.{Group, Actor}
import com.mygdx.game.ScreenResources
import com.mygdx.game.gui.{SlotCardActors, Board, Run, Focus}
import priv.sp._

class SlotComponent(
  var slotnum : Int,
  var playerId : PlayerId,
  val slotCardActors : SlotCardActors,
  val group : Group,
  val game : GameSession,
  resources : ScreenResources) extends Component with Drawable {

  def actor = slotCardActors.cardGroup

  def focus(): Actor = {
    actor addAction new Focus
    actor
  }

  def direction = if (playerId == game.myPlayerId) 1 else -1
  def run(): Actor = {
    actor addAction new Run(direction, resources.config.getConfig("card.run"))
    actor
  }

  def draw(batch : Batch) = group.draw(batch, 1)

  def fade() : Actor = {
    val action = new AlphaAction
    action setDuration 0.1f
    action setAlpha 0
    actor addAction action
    actor
  }
}


class SlotSystem(batch : Batch) extends BaseSystem {
  protected val family = Family.all(classOf[SlotComponent]).get()
  private val sm = ComponentMapper.getFor(classOf[SlotComponent])

  def findEntity(slotnum : Int, playerId : Int) : Option[(Entity, SlotComponent)] = {
    val results = entities.toArray().toStream
      .collect { case entity => (entity, sm get entity) }
      .filter { case (_, slotComponent) => slotComponent.playerId == playerId && slotComponent.slotnum == slotnum }

    if (results.size > 1) println("Found more than 1 entity at " + slotnum + " for player " + playerId)
    results.headOption
  }

  def listEntities(playerId : PlayerId) : List[(Entity, SlotComponent)] = {
    entities.toArray()
      .collect { case entity => (entity, sm get entity) }
      .filter { case (_, slotComponent) => slotComponent.playerId == playerId }
      .toList
  }

  def synchronizeState(game : GameSession, resources : ScreenResources, board : Board) = {
    for {
      playerId <- playerIds
      p = game.state.players(playerId)
      slotnum  <- baseSlotRange
    } {
      (findEntity(slotnum, playerId), p.slots get slotnum) match {
        case (None, Some(slotState)) =>
          println("add missing " + slotnum + " to " + playerId)
          val source = com.mygdx.game.gui.getCoord(board.slotPanels(playerId).slots(slotnum).group).add(15, 15)
          val (entity, _) = SlotCardActors.createEntity(slotnum, playerId, slotState, game, resources, source)
          resources.engine addEntity entity
        case (Some((entity, _)), None) =>
          // FIXME ? when summoning state is not filled yet
          println("!!! removed entity at " + slotnum + "/" + playerId)
          resources.engine removeEntity entity
        case (Some((_, slotComponent)), Some(slotState)) =>
          slotComponent.slotCardActors update slotState
        case _ =>
      }
    }
  }

  override def update(deltaTime : Float) = {
    val ite = entities.iterator()
    while (ite.hasNext) {
      val entity = ite.next()
      val slotComponent = sm get entity
      slotComponent.group act deltaTime
    }
  }
}
