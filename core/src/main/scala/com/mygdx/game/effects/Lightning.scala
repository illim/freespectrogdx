package com.mygdx.game.effects

import scala.util.Random
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType
import com.badlogic.ashley.core.Entity
import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.math.Vector2
import com.mygdx.game.ScreenResources
import com.mygdx.game.component._

object Lightning {

  def apply(resources : ScreenResources, points: Vector2*) = {
    val entity = new Entity()
    val timedComponent = new TimedComponent(1.5f)
    val lightning = new Lightning(timedComponent, resources, points : _*)
    entity add timedComponent
    entity add new VisualComponent(lightning)
    (entity, timedComponent.prom)
  }
}


class Lightning(timedComponent : TimedComponent, resources : ScreenResources, points: Vector2*)
  extends Drawable {

  import resources.shapes

  val lines = split(
    points
      .zip(points.tail)
      .map { case (p1, p2) ⇒ new Line(p1, p2) },
    5)

  def draw(batch : Batch) {
    shapes.setProjectionMatrix(resources.stage.getCamera.combined)
    shapes setColor new Color(1, 1, 1, 1 - (timedComponent.time / timedComponent.duration))
    shapes begin ShapeType.Line
    lines foreach { l ⇒ shapes.line(l.a, l.b) }
    shapes.end()
  }

  case class Line(a: Vector2, b: Vector2) {
    def smallOff = 0.4f + 0.2f * Random.nextFloat
    def split() = {
      val dir = b.cpy() sub a
      val diro = new Vector2(dir.x * smallOff, dir.y * smallOff)
      val c = a.cpy() add diro
      List(Line(a, c), Line(c, b))
    }
  }

  def split(ss: Seq[Line], n: Int): Seq[Line] = {
    if (n == 0) {
      ss
    } else {
      split(ss flatMap (_.split()), n - 1)
    }
  }
}
