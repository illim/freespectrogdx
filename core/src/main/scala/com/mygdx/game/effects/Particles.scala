package com.mygdx.game.effects

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g2d.{TextureAtlas, ParticleEffect}
import com.badlogic.gdx.math.Vector2
import com.mygdx.game.component.TimedComponent
import priv.util.ResourceCache

class MyParticleEffects(atlas : TextureAtlas) extends ResourceCache[String, ParticleEffect] {

  def load(name : String) = {
    val effect = new ParticleEffect
    effect.load(Gdx.files.internal("particle/"+name+".p"), atlas)
    effect
  }

  def dispose() : Unit = {
    resources.values.foreach(_.dispose())
  }

}

/**
 * old particle emitter
 */
trait Particle {
  def startPos: Vector2
  def lifeTime: Float
  def creationTime : Float
  def render(t: Float)
}

trait Emitter {
  def timedComponent : TimedComponent
  def rate: Float

  def maxTime: Float
  def build(time: Float): Particle
  def maxParticles : Int
  var particles = Vector.empty[Particle]
  var lastEmit = 0f

  def render() {
    if (timedComponent.time - lastEmit > rate && particles.size < maxParticles) {
      val p = build(timedComponent.time)
      particles = particles :+ p
      lastEmit = timedComponent.time
    }
    particles foreach { p ⇒
      if (timedComponent.time - p.creationTime > p.lifeTime) {
        particles = particles filterNot (_ == p)
      } else {
        p.render(timedComponent.time - p.creationTime)
      }
    }
  }
}