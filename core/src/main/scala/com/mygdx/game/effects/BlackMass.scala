package com.mygdx.game.effects


import scala.util.Random
import com.badlogic.ashley.core.Entity
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import com.badlogic.gdx.math.Vector2
import com.mygdx.game.ScreenResources
import com.mygdx.game.component.{Drawable, VisualComponent, TimedComponent}


class Mass(val startPos: Vector2, dir: Vector2, yLimit: Float, val creationTime : Float, shapes : ShapeRenderer) extends Particle {
  val lifeTime = 0.8f
  def render(t: Float) : Unit = {
    val a = startPos.cpy() add (dir.cpy() scl (t * 10))
    var b = a.cpy() add (dir.cpy() scl 2)
    if ((dir.y < 0 && b.y < yLimit) || (dir.y > 0 && b.y > yLimit)) {
      b = new Vector2(a.x - 1 + Random.nextInt(3), a.y - 1 + Random.nextInt(3))
    }
    shapes.line(a, b)
  }
}
class MassEmitter(pos: Vector2, target: Vector2, targetDim : Vector2, val timedComponent : TimedComponent, shapes : ShapeRenderer)
  extends Emitter {

  val rate = 0.02f
  val maxTime = timedComponent.duration * 0.8f
  val maxParticles = 200
  def getRandomPoint = new Vector2(target.x + Random.nextFloat()* targetDim.x, target.y + Random.nextFloat() * targetDim.y)
  val down = target.y > pos.y
  def getDir(p: Vector2) = (pos.cpy() sub p) scl 0.05f
  def build(time: Float) = {
    val p = getRandomPoint
    new Mass(p, getDir(p), pos.y, time, shapes)
  }
}

object BlackMass {

  def apply(resources : ScreenResources, source : Vector2, target : Vector2, targetDim : Vector2) = {
    val entity = new Entity()
    val timedComponent = new TimedComponent(1f)
    val blackMass = new BlackMass(source, target, targetDim, timedComponent, resources)
    entity add timedComponent
    entity add new VisualComponent(blackMass)
    (entity, timedComponent.prom)
  }
}

class BlackMass(pos: Vector2, target : Vector2, targetDim : Vector2, timedComponent : TimedComponent, resources : ScreenResources)
  extends Drawable {
  import resources.shapes

  val emitter = new MassEmitter(pos, target, targetDim, timedComponent, shapes)

  def draw(batch : Batch) {
    shapes.setProjectionMatrix(resources.stage.getCamera.combined)
    shapes setColor new Color(0.6f, 0.6f, 0.6f, 1)
    shapes begin ShapeType.Line
    emitter.render()
    shapes.end()
  }
}
