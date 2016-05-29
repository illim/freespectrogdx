package com.mygdx.game.effects

import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.{VertexAttribute, Mesh, Color}
import com.typesafe.config.Config

import collection._
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import priv.util.ResourceCache

class Shaders extends ResourceCache[String, Shader] {

  def load(path: String) = resources.getOrElseUpdate(path, new BasicShader(path))
  def dispose() = resources.values foreach (res ⇒ res.program.dispose())

  class BasicShader(name: String) extends Shader {
    val program = Shader create ("data/shaders/" + name)
  }
}

object Shader {

  def create(name: String) = {
    val program = new ShaderProgram(
      Gdx.files.internal(name + ".vert"),
      Gdx.files.internal(name + ".frag"))
    if(!program.isCompiled()) {
      Gdx.app.log("Problem loading shader " + name + ":", program.getLog());
    }
    program
  }
}

trait Shader {
  var pedantic = true

  def program: ShaderProgram

  def used[A](f: ⇒ A) = {
    ShaderProgram.pedantic = pedantic
    program.begin()
    try f finally program.end()
  }


  def getUniformLocations(names: String*): List[Int] = names.map(getUniformLocation(_))(breakOut)
  def getUniformLocation(name: String) = {
    val res = program.getUniformLocation(name)
    require(res != -1, name + " doesn't exists")
    res
  }
}


class SelectedShader(name: String, config : Config) extends Shader {
  pedantic = false
  val program = Shader create ("data/shaders/" + name)
  val size :: cursor :: _ = getUniformLocations("size", "cursor")

  val animLength = config getInt "animlength"
  val sizeConfig = config getInt "size"
  val offsetx = config getInt "offset.x"
  val offsety = config getInt "offset.y"

  val mesh = createPoliQuad(offsetx, offsety, sizeConfig, sizeConfig)

  def createPoliQuad(x : Float, y : Float, w : Float, h : Float) = {
    val verts = Array[Float](
      x    , y + h, Color.toFloatBits(255, 255, 255, 128),
      x    , y    , Color.toFloatBits(255, 255, 255, 128),
      x + w, y + h, Color.toFloatBits(255, 255, 255, 128),
      x + w, y    , Color.toFloatBits(255, 255, 255, 128))


    val mesh = new Mesh( true, 4, 4,  // static mesh with 6 vertices and no indices
      new VertexAttribute( Usage.Position, 2, ShaderProgram.POSITION_ATTRIBUTE ),
      new VertexAttribute( Usage.ColorPacked, 4, ShaderProgram.COLOR_ATTRIBUTE ))

    mesh.setVertices( verts )
    mesh.setIndices(Array[Short]( 0, 1, 2, 3 ))
    mesh
  }

}

class FadeShader(name: String) extends Shader {
  val program = Shader create name
  val fact = getUniformLocation("fact")
}

class RippleShader extends Shader {
  val name = "ripple"
  val program = Shader create name
  val cursor = getUniformLocation("cursor")
}