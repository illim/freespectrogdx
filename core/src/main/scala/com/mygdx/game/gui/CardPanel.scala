package com.mygdx.game.gui

import com.badlogic.gdx.graphics.Color
import com.badlogic.gdx.scenes.scene2d.{InputEvent, Actor}
import com.badlogic.gdx.scenes.scene2d.ui.Table
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener
import com.mygdx.game._
import priv.sp._

class CardPanel(playerId: PlayerId,
                game: GameSession,
                descriptionPanel : DescriptionPanel,
                selectedEffectActor : SelectedEffectActor,
                hoveredActor : HoveredActor,
                resources : ScreenResources) {

  private val houseCardButtons = game.desc.players(playerId).houses.zipWithIndex map {
    case (houseDesc, idx) ⇒
      def getCard(i: Int) : Option[CardDesc] = {
        val cards = game.state.players(playerId).desc.get.houses(idx).cards
        if (i < cards.size) Some(cards(i)) else None
      }
      def getHouseState = game.state.players(playerId).houses(idx)
      new HouseLabel(getHouseState.mana, houseDesc.house, resources) -> (0 to 3).map { i ⇒
        new CardButton(getCard(i), getHouseState, houseDesc, resources)
      }
  }
  val cardButtons = houseCardButtons flatMap (_._2)
  val houseLabels = houseCardButtons map (_._1)
  var lastSelected = Option.empty[CardButton]

  def init(commandRecorder : CommandRecorder) {
    if (playerId == game.myPlayerId) {
      cardButtons foreach { cardButton ⇒
        cardButton.visible = true
        cardButton.group.addListener(new ClickListener() with HoverToDesc {
          def descPanel = descriptionPanel
          def getDescription = getCardDescription(cardButton)

          override def enter(event: InputEvent, x: Float, y: Float, pointer : Int, fromActor : Actor) : Unit = {
            super.enter(event, x, y, pointer, fromActor)
            if ( ! cardButton.selected && cardButton.isActive ) {
              val pos = getCoord(cardButton.group)
              hoveredActor.effect.setPosition(pos.x + 40, pos.y + 50)
              hoveredActor.effect.reset()
              hoveredActor.enabled = true
            }
          }
          override def exit(event: InputEvent, x: Float, y: Float, pointer : Int, fromActor : Actor) : Unit = {
            super.exit(event, x, y, pointer, fromActor)
            hoveredActor.enabled = false
          }

          override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
            if (cardButton.isActive) {
              cardButton.cardActorsOption foreach { h ⇒
                commandRecorder setCommand Command(game.myPlayerId, h.desc.card, None, h.desc.cost)
                if (h.desc.card.inputSpec.isDefined) {
                  lastSelected foreach { b =>
                    b.selected = false
                  }

                  selectedEffectActor.groupCoord = getCoord(cardButton.group)
                  selectedEffectActor.selected = true
                  cardButton.selected = true
                  lastSelected = Some(cardButton)
                }
              }
            }
          }
        })
      }
    } else {
      cardButtons foreach { cardButton ⇒
        cardButton.visible = true
        cardButton.group.addListener(new ClickListener() with HoverToDesc {
          def descPanel      = descriptionPanel
          def getDescription = getCardDescription(cardButton)
        })
      }
    }
  }

  houseLabels foreach { houseLabel =>
    houseLabel.panel.addListener(new ClickListener() with HoverToDesc {
      def descPanel      = descriptionPanel
      def getDescription = Some(Description.houseToDesc(houseLabel.house))

      override def enter(event: InputEvent, x: Float, y: Float, pointer : Int, fromActor : Actor) : Unit = {
        houseLabel.label setColor Color.BLUE
        super.enter(event, x, y, pointer, fromActor)
      }
      override def exit(event: InputEvent, x: Float, y: Float, pointer : Int, fromActor : Actor) : Unit = {
        houseLabel.label setColor Color.WHITE
        super.exit(event, x, y, pointer, fromActor)
      }

      override def clicked(event: InputEvent, x: Float, y: Float): Unit = {
        resources.stage addActor new HouseDescription(houseLabel.house, game.state, playerId, resources)
      }
    })
  }

  val panel = new Table
  val houseActors : Seq[Actor] = houseCardButtons map { case (houseLabel, _) => houseLabel.panel }
  val cardActors : List[Seq[Actor]] = (0 to 3).map { i =>
    val cards :Seq[CardButton] = houseCardButtons.map{_._2(i)}
    cards.map(_.group : Actor)
  }.toList
  val actorRows : List[Seq[Actor]] = if (playerId == game.myPlayerId) houseActors :: cardActors else cardActors :+ houseActors

  actorRows foreach { actors =>
    actors foreach (a => panel.add(a))
    panel.row()
  }

  panel.pack()

  def refresh(silent : Boolean): Unit ={
    houseLabels foreach (_.manaDamageable.refresh(silent))
    cardButtons foreach (_.refresh())
    cardButtons foreach { cb ⇒
      cb.visible = (playerId == game.myPlayerId
      || (cb.cardActorsOption.isDefined && visibleCards.contains(cb.cardActorsOption.get.desc.card)))
    }
  }

  def getCardDescription(cardButton: CardButton) = {
    cardButton.cardActorsOption collect { case h if cardButton.visible =>
      Description.cardToDesc(game.state, playerId, h.desc.card)
    }
  }

  def setEnabled(flag: Boolean) {
    cardButtons foreach { btn ⇒
      btn.enabled = flag
    }
    lastSelected foreach (_.selected = false)
    selectedEffectActor.selected = false
    lastSelected = None
  }

  var visibleCards = Set.empty[Card]
  def addVisibleCard(c: Card) {
    visibleCards = visibleCards + c
  }

  def findCardButtonOf(card: Card) = {
    val someCard = Some(card)
    cardButtons.find(_.getCardOption == someCard)
  }

}
