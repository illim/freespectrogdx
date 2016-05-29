package com.mygdx.game.component

import com.badlogic.ashley.core._
import com.badlogic.ashley.systems.IteratingSystem
import com.badlogic.ashley.utils.ImmutableArray
import com.badlogic.gdx.graphics.Camera
import com.badlogic.gdx.graphics.g2d.Batch

class TimedSystem(engine : Engine) extends IteratingSystem(Family.all(classOf[TimedComponent]).get()) {
  private val tm = ComponentMapper getFor classOf[TimedComponent]

  override def processEntity(entity : Entity, deltaTime : Float) = {
    val timedComponent = tm get entity
    timedComponent update deltaTime
    if (timedComponent.isCompleted){
      engine removeEntity entity
      timedComponent.prom.success(())
    }
  }
}

class ScriptSystem extends IteratingSystem(Family.all(classOf[ScriptComponent]).get()) {
  private val sm = ComponentMapper.getFor(classOf[ScriptComponent])

  override def processEntity(entity : Entity, deltaTime : Float) = {
    (sm get entity).script update deltaTime
  }
}

trait BaseSystem extends EntitySystem {
  var entities = new ImmutableArray[Entity](new com.badlogic.gdx.utils.Array[Entity]())

  override def addedToEngine(engine : Engine) {
    entities = engine getEntitiesFor family
  }

  protected def family : Family
}

class RenderSystem(batch : Batch, camera : Camera) extends BaseSystem {
  protected val family = Family.all(classOf[VisualComponent]).get()
  private val vm = ComponentMapper getFor classOf[VisualComponent]

  override def update(deltaTime : Float) = {

    batch.begin()

    val ite = entities.iterator()
    while (ite.hasNext) {
      val e = ite.next()
      (vm get e).drawable draw batch
    }

    batch.end()
  }

}

class ParticleSystem(engine : Engine) extends IteratingSystem(Family.all(classOf[ParticleComponent]).get()) {
  private val pm = ComponentMapper getFor classOf[ParticleComponent]

  override def processEntity(entity : Entity, deltaTime : Float) = {
    val particleComponent = pm get entity

    particleComponent update deltaTime
    if (particleComponent.isComplete){
      particleComponent.endPromise.success(())
      if (particleComponent.isEndingEntity){
        engine removeEntity entity
      } else {
        entity remove classOf[ParticleComponent]
      }
    }
  }
}
