package com.mygdx.game.actor

import com.badlogic.gdx.graphics.VertexAttributes.Usage
import com.badlogic.gdx.graphics.g2d.Batch
import com.badlogic.gdx.graphics.{GL20, VertexAttribute, Mesh, Color}
import com.badlogic.gdx.graphics.glutils.ShaderProgram
import com.badlogic.gdx.scenes.scene2d.Actor
import com.mygdx.game.ScreenResources

class Repere(resources : ScreenResources) extends Actor {

  val repere = createRepere()

  override def draw(batch : Batch, parentAlpha : Float): Unit = {
    batch.end()
    batch.begin()
    val repereShader =  resources.effectResources.repere
    import repereShader.program

    batch.disableBlending()
    program.begin()
    program.setUniformMatrix("u_projTrans", batch.getProjectionMatrix)
    repere.render(program, GL20.GL_LINES, 0, 4)
    program.end()
    batch.enableBlending()
    batch.end()
    batch.begin()
  }

  private def createRepere() = {
    val verts = Array[Float](
      1    , 1   , Color.toFloatBits(0, 255, 0, 255),
      100  , 1   , Color.toFloatBits(0, 255, 0, 255),
      1    , 1   , Color.toFloatBits(255, 0, 0, 255),
      1    , 100 , Color.toFloatBits(255, 0, 0, 255))

    val mesh = new Mesh( true, 4, 4,  // static mesh with 6 vertices and no indices
      new VertexAttribute( Usage.Position, 2, ShaderProgram.POSITION_ATTRIBUTE ),
      new VertexAttribute( Usage.ColorPacked, 4, ShaderProgram.COLOR_ATTRIBUTE ))

    mesh.setVertices( verts )
    mesh.setIndices(Array[Short]( 0, 1, 2, 3 ))
    mesh
  }
}
