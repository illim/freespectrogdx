package com.mygdx.game.gui

import com.badlogic.ashley.core.Entity
import com.badlogic.gdx.Gdx
import com.badlogic.gdx.graphics.g2d.{Batch, ParticleEffect}
import com.badlogic.gdx.math.Vector2
import com.mygdx.game.ScreenResources
import com.mygdx.game.component._
import com.mygdx.game.effects.{SoundEntity, BlackMass, Lightning}
import priv.sp._

import scala.concurrent.Future

class SpellCast(board: Board, game : GameSession, resources : ScreenResources) {
  import game.sp.houses._

  def cast(c : Command) : Option[Future[Unit]] = {
    val particles = resources.effectResources.particles

    playSound(c.card)
    if (c.card == Air.cards(5)) {
      getSourceCoord(c) map { source =>
        val playerPos = getCoord(getPlayerLabel(other(c.player))).add(30, 20)
        val (entity, prom) = Lightning(resources, source, playerPos)
        resources.engine addEntity entity
        prom.future
      }
    } else if (c.card == Air.cards(2)) {
      getSourceCoord(c) map { source =>
        val playerPos = getCoord(getPlayerLabel(other(c.player))).add(30, 20)
        val target = getTargetCoord(c).get
        val (entity, prom) = Lightning(resources, source, target, playerPos)
        resources.engine addEntity entity
        prom.future
      }
    } else if (c.card == Air.cards(7)) {
      val playerPos = getCoord(getPlayerLabel(other(c.player))).add(30, 20)
      val points = (0 to 5) map { i =>
        getCoord(board.slotPanels(other(c.player)).slots(i).group).add(50, 50 - 20 * (i % 2))
      }
      val (entity, prom) = Lightning(resources, (playerPos +: points).reverse : _*)
      resources.engine addEntity entity
      Some(prom.future)
    } else if (c.card == darkPriest.DarkPriest.cards(2)) {
      getTargetCoord(c) map { source =>
        val target = getCoord(getLeftSlot(other(c.player)))
        val (entity, prom) = BlackMass(resources, source, target, new Vector2(500, 100))
        resources.engine addEntity entity
        prom.future
      }
    } /**else if (c.card == warp.Warp.cards(1)) {
      val (entity, prom) = EarthQuake(resources, other(c.player))
      resources.engine addEntity entity
      Some(prom.future)
    }*/ else {
      val effectOption : Option[ParticleEffect] = if (c.card == Earth.cards(1)) {
        val effect = particles.get("nature_heal")
        val pos = getTargetCoord(c).get
        effect.setPosition(pos.x, pos.y)
        Some(effect)
      } else if (c.card == Fire.cards(5)) {
        val effect = particles.get("fire_wall")
        val pos = getCoord(getLeftSlot(other(c.player)))
        effect.setPosition(pos.x, pos.y)
        Some(effect)
      } else if (c.card == faerieDruid.FaerieDruid.cards(1)) {
        val effect = particles.get("faerie_fire")
        val pos = getCoord(getLeftSlot(other(c.player)))
        effect.setPosition(pos.x, pos.y)
        Some(effect)
      } else if (c.card == Water.cards(7)) {
        val effect = particles.get("acid_rain")
        val pos = getCoord(getLeftSlot(opponent))
        effect.setPosition(pos.x, pos.y)
        Some(effect)
      } else if (c.card == limbo.Limbo.cards(4)) {
        val effect = particles.get("nether_grasp")
        val pos = getCoord(getLeftSlot(c.player))
        effect.setPosition(pos.x, pos.y)
        Some(effect)
      }  else if (c.card == limbo.Limbo.cards(6)) {
        val effect = particles.get("redemption")
        val pos = getTargetCoord(c).get
        effect.setPosition(pos.x, pos.y)
        Some(effect)
      }else if (c.card == vampire.Vampire.cards(4)) {
        val effect = particles.get("blood_burst")
        val pos = getTargetCoord(c).get
        effect.setPosition(pos.x, pos.y)
        Some(effect)
      } else None

      effectOption map { effect =>
        val (entity, particleComponent) = createEffectEntity(effect)
        resources.engine addEntity entity
        particleComponent.endPromise.future
      }
    }
  }

  def getSourceCoord(c : Command) = {
    val cardButtonOption = board.cardPanels(c.player).findCardButtonOf(c.card)
    cardButtonOption map { b => getCoord(b.group) }
  }

  def getTargetCoord(c : Command) = findTargetSlot(c) map { b => withSlotOffet(getCoord(b.group)) }

  def findTargetSlot(c : Command) = {
    c.input map { num =>
      val targetPlayer = if (c.card.inputSpec.get.isInstanceOf[OwnerInputSlot]) c.player else other(c.player)
      board.slotPanels(targetPlayer).slots(num.num)
    }
  }

  def getPlayerLabel(playerId: PlayerId) = {
    board.slotPanels(playerId).lifeLabel.label
  }

  def getLeftSlot(playerId : PlayerId) = {
    board.slotPanels(playerId).slots(0).group
  }

  def withSlotOffet(vec : Vector2) = vec.add(50f, 50f)

  def createEffectEntity(effect : ParticleEffect) = {
    val entity = new Entity

    val duration = math.min(3, effect.getEmitters.toArray.map(_.getDuration.getLowMax).max)
    val particleComponent = new ParticleComponent(effect, duration, isEndingEntity = true)
    entity add particleComponent
    entity add new VisualComponent(new Drawable{ def draw(batch : Batch) = effect.draw(batch) })
    (entity, particleComponent)
  }

  def playSound(card : Card) = {
    val soundName = card.name.toLowerCase.replaceAll(" ", "")
    val key = "sounds."+soundName
    if (resources.config hasPath key) {
      val path = "sounds/" + (resources.config getString key)
      val sound = Gdx.audio.newSound(Gdx.files.internal(path))
      resources.engine addEntity SoundEntity(sound, 4, resources)
    }
  }
}
