package com.mygdx.game


import com.badlogic.ashley.core.{Engine, Entity}
import com.badlogic.gdx.graphics.Texture.TextureFilter
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator.FreeTypeFontParameter
import com.mygdx.game.component._

import collection.JavaConverters._
import com.badlogic.gdx.graphics.g2d.{Batch, TextureAtlas}
import com.badlogic.gdx.graphics.glutils.ShapeRenderer
import com.badlogic.gdx.scenes.scene2d.{Actor, Stage}
import com.badlogic.gdx.scenes.scene2d.ui.{Label, Skin}
import com.badlogic.gdx.Gdx
import com.mygdx.game.effects._
import com.mygdx.game.net.NetClient
import com.typesafe.config.ConfigFactory
import priv.sp.I18n

import scala.concurrent.Promise
import scala.util.control.NonFatal

class ScreenResources(val storage : Storage) extends GraphicResourceBase {

  var config   = ConfigFactory.load()
  val stage    = new Stage()
  val batch    = stage.getBatch
  val renderer = new ShapeRenderer()
  val atlas    = new TextureAtlas(Gdx.files.external(".freespectro/files/"+storage.cardTheme+"/pack/images.pack.atlas"))

  val commonSkin = loadSkin("font3")
  var skin     = if (I18n.isRussian) commonSkin else loadSkin("font")
  var skin2    = if (I18n.isRussian) commonSkin else loadSkin("font2")
  val engine   = new Engine()
  val renderSystem   = new RenderSystem(batch, stage.getCamera)
  val scriptSystem   = new ScriptSystem()
  val particleSystem = new ParticleSystem(engine)
  val slotSystem     = new SlotSystem(batch)
  val timedSystem    = new TimedSystem(engine)
  var clientOption   = Option.empty[NetClient]

  val shapes = new ShapeRenderer()

  val beforeProcess = new BeforeProcess

  atlas.getTextures.asScala.foreach(_.setFilter(TextureFilter.Linear, TextureFilter.Linear)) // smooth textures, to fix glitches when card moves and viewport resizing

  val effectResources = new EffectResources(new Shaders, this)

  def clear() : Unit = {
    stage.clear()
    engine.removeAllEntities()
    Gdx.input setInputProcessor stage
  }

  def initGameSystems(): Unit = {
    engine addSystem scriptSystem
    engine addSystem timedSystem
    engine addSystem particleSystem
    engine addSystem slotSystem
    engine addSystem renderSystem
  }

  def removeGameSystems() : Unit = {
    engine removeSystem scriptSystem
    engine removeSystem timedSystem
    engine removeSystem particleSystem
    engine removeSystem slotSystem
    engine removeSystem renderSystem
  }

  def addEndMessage(msg : String) = beforeProcess invoke {
    val endMessageLabel = new Label(msg, skin)
    endMessageLabel.setPosition(10, 550)
    val endMessage = new Entity
    endMessage add new VisualComponent(new Drawable(){
      override def draw(batch: Batch): Unit = {
        endMessageLabel.draw(batch, 1)
      }
    })
    engine addEntity endMessage
  }

  def reload() = {
    try {
      ConfigFactory.invalidateCaches()
      config = loadConfig()
      skin = loadSkin("font")
      skin2 = loadSkin("font2")
    } catch { case NonFatal(t) => t.printStackTrace() }
  }

  sys.addShutdownHook({
    disconnectIfNeeded()
  })

  def dispose(): Unit ={
    disconnectIfNeeded()
    effectResources.shaders.dispose()
    effectResources.particles.dispose()
    stage.dispose()
    batch.dispose()
    renderer.dispose()
    skin.dispose()
    atlas.dispose()
  }

  def disconnectIfNeeded() : Unit = {
    clientOption.foreach(_.release())
    clientOption = None
  }

  def configure(actor : Actor, name : String) = {
    if (config hasPath name) {
      val c = config getConfig name
      if (c hasPath "pos") {
        val pos = (c getDoubleList "pos").asScala.toList.map(_.toFloat)
        if (pos.size == 2) actor.setPosition(pos(0), pos(1))
        else actor.setPosition(pos(0), pos(1), pos(2).toInt)
      }
      if (c hasPath "color") {
        val col = (c getDoubleList "color").asScala.toList.map(_.toFloat)
        val alpha = if (col.size == 4) col(3) else 1f
        actor.setColor(col(0), col(1), col(2), alpha)
      }
    }
  }

  def loadSkin(fontKey : String) = {
    val font = generateFont(fontKey)
    val skin = new Skin()
    skin addRegions new TextureAtlas(Gdx.files.internal("data/uiskin.atlas"))
    skin.add("default-font", font)
    skin load Gdx.files.internal("data/uiskin.json")
    skin
  }

  def generateFont(fontKey : String) = {
    val c = config getConfig fontKey
    val generator = new FreeTypeFontGenerator(Gdx.files.internal(c getString "name"))
    val parameter = new FreeTypeFontParameter()
    val fontChars = if (I18n.isRussian) {
      "абвгдеёжзийклмнопрстуфхцчшщъыьэюяabcdefghijklmnopqrstuvwxyzАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789][_!$%#@|\\/?-+=()*&.;:,{}\"´`'<>"
    } else {
      FreeTypeFontGenerator.DEFAULT_CHARS
    }
    parameter.characters    = fontChars
    parameter.size          = c getInt "size"
    parameter.shadowOffsetX = c getInt "shadowOffsetX"
    parameter.shadowOffsetY = c getInt "shadowOffsetY"
    val font = generator generateFont parameter
    generator.dispose()
    font
  }

}

class BeforeProcess {
  @volatile private var task = Option.empty[() => Unit]
  private val taskLock = new Object

  def invoke[A](f : => A) : A = {
    val prom = Promise[A]()
    val func = () => {
      val res = f
      prom success res
      ()
    }
    taskLock synchronized {
      task = Some(func)
      taskLock.wait()
      prom.future.value.get.get
    }
  }

  def apply() = {
    taskLock synchronized {
      task foreach { func =>
        func()
        task = None
        taskLock.notifyAll()
      }
    }
  }
}
