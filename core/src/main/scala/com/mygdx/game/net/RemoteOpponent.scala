package com.mygdx.game.net

import priv.sp._
import priv.util._

object GameSeed {

  def create(resources : GameResources) = {
    val seed = System.currentTimeMillis
    val shuffle = new CardShuffle(resources.sp.houses)
    val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get(resources.resolveChoices, owner)
    val state = GameState(List(PlayerState.init(p1State, p1Desc), PlayerState.init(p2State, p2Desc)))

    GameSeed(seed, GameDesc(Vector(p1Desc, p2Desc)), state)
  }
}

case class GameSeed(seed : Long, desc : GameDesc, gameState : GameState )


class RemoteOpponent(
  resources : GameResources,
  val client : NetClient,
  val name : String,
  val playerId : PlayerId,
  val startingPlayer : PlayerId,
  gameSeed : GameSeed) extends GameServer {

  val seed      = gameSeed.seed
  val initState = gameSeed.gameState
  val desc      = gameSeed.desc

  def submitCommand(commandOption: Option[Command]) : Unit = {
    client.proxyMessage(commandOption)
  }
  
  def waitNextCommand(c: TVar[Option[Option[Command]]], state: GameState) : Unit = {
    val msg = client.messageQueue.take()
    msg match {
      case commandOption : Option[Command] => c set Some(commandOption)
      case _ => println("Unknown msg " + msg)
    }
  }

}
