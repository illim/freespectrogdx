package com.mygdx.game.component

import scala.concurrent.Promise
import com.badlogic.ashley.core.Component
import com.badlogic.gdx.graphics.g2d.{Batch, ParticleEffect}

class TimedComponent(val duration : Float, var time : Float = 0f, val prom : Promise[Unit] = Promise[Unit]()) extends Component {
  def update(deltaTime : Float) = {
    time = time + deltaTime
  }
  def isCompleted = time >= duration
}

trait Drawable {
  def draw(batch : Batch) : Unit
}
class VisualComponent(val drawable : Drawable) extends Component

trait Scriptable {
  def update(delta : Float) : Unit
}

class ScriptComponent(val script : Scriptable) extends Component

class ParticleComponent(
  val effect : ParticleEffect,
  val duration : Float,
  val isEndingEntity : Boolean = false,
  val endPromise : Promise[Unit] = Promise[Unit]() ) extends Component {

  var time : Float = 0f

  def update(deltaTime : Float) = {
    time = time + deltaTime
    effect.update(deltaTime)
  }

  def isComplete = effect.isComplete || time >= duration

  effect.reset()
  effect.start()
}
