package com.mygdx.game.effects

import com.mygdx.game.ScreenResources
import com.badlogic.ashley.core.Entity
import com.badlogic.gdx.audio.Sound
import com.mygdx.game.component.{Scriptable, ScriptComponent, TimedComponent}
import scala.concurrent.ExecutionContext.Implicits.global


object SoundEntity {

  def apply(sound : Sound, duration : Float, resources : ScreenResources) = {
    val timed = new TimedComponent(duration)
    timed.prom.future.foreach { _ =>
      sound.stop()
      sound.dispose()
    }
    val script = new ScriptComponent(new FadingSound(sound, timed, resources))
    val entity = new Entity
    entity add timed
    entity add script
    entity
  }
}

class FadingSound(sound : Sound, timed : TimedComponent, resources : ScreenResources) extends Scriptable {
  val soundId = sound play 1
  val fadeTime = (resources.config getDouble "sounds.fadePercent" ).toFloat * timed.duration
  val fadeStart = timed.duration - fadeTime

  def update(delta : Float) : Unit = {
    if (timed.time > fadeStart) {
      sound.setVolume(soundId, math.max(0, 1 - (timed.time - fadeStart) / fadeTime))
    }
  }
}
