package com.mygdx.game.net

import java.io._
import java.net._

import collection._
import scala.concurrent.Promise
import scala.util.{Failure, Success}
import priv.util.Utils.thread
import com.mygdx.game.{RemoteGameScreenContext, Screens}
import priv.sp._
import priv.util.Log
import java.nio.charset.StandardCharsets

import scala.concurrent.ExecutionContext.Implicits.global

class NetClient(host : String, port : Int, val user : String,
                screens : Screens,
                logMsg : String => Unit,
                logDuelRequest : String => Unit,
                setPlayerList : List[PlayerInfo] => Unit) {

  import screens._

  val socket  = new Socket(host, port)
  val out     = socket.getOutputStream()
  val in      = socket.getInputStream()
  val pout    = new PrintWriter(out, true)
  val pending = mutable.HashMap.empty[Int, Promise[Any]]
  val messageQueue = new java.util.concurrent.LinkedBlockingQueue[Any]
  val log     = new Log(this)
  def kryo    = GameKryoInstantiator.kryo
  var currentGame = Option.empty[RemoteGameScreenContext]

  var running = true
  thread("net client") {
    while (running) {
      getMessage() foreach { message =>
        if (message.header.answerId > 0) {
          pending.get(message.header.answerId) match {
            case Some(prom) => prom.success(message)
            case None => handle(message)
          }
        } else {
          handle(message)
        }
      }
    }
    release()
  }

  def handle( message : Message ) = {
    log.debug("Handle " + message)
    message.header.messageType match {
      case MessageType.Welcome =>
        message.body foreach ( x => logMsg(new String(x, StandardCharsets.UTF_8)) )
        send(Message(Header(MessageType.Name), Some(user.getBytes(StandardCharsets.UTF_8))))
      case MessageType.ListPlayers =>
        message.body foreach { bytes =>
          setPlayerList(new String(bytes).split(";").toList.map(PlayerInfo.decode))
        }
      case MessageType.NewGame =>
        // ask opponent name and house preferences
        val prom = proxyAsk(new AskOpponentInfo)
        prom.future onComplete {
          case Failure(t) => logMsg(t.getMessage) ; t.printStackTrace()
          case Success(OpponentInfo(oppName, oppHouses, oppChecksum)) =>
            if (oppChecksum != screenResources.storage.checksum) {
              val msg = "Checksum mismatch " + screenResources.storage.checksum + "/" + oppChecksum
              log.error(msg)
              logMsg(msg)
              Thread.sleep(1000)
            }
            messageQueue.clear()
            // a new game has been requested
            val seed = GameSeed.create(gameResources, user, oppName, oppHouses)
            // send the seed to the opponent
            proxyMessage(seed)
            screenResources.beforeProcess invoke {
              gameScreen.select()
              val opp = new RemoteOpponent(gameResources, this, oppName, opponent, owner, seed)
              setGame(new RemoteGameScreenContext(gameScreen, opp))
            }
          case msg => log.debug("unknown msg " + msg)
        }
      case MessageType.ExitDuel =>
        screenResources.beforeProcess.invoke {
          messageQueue put GameSeed.PoisonPill
          currentGame foreach (_.gameInit.releaseLocks())
          gameScreen.returnToStart()
        }
      case MessageType.RequestDuel =>
        message.body foreach { bytes =>
          logDuelRequest(new String(bytes, StandardCharsets.UTF_8))
        }
      case MessageType.Proxy =>
        message.body foreach { bytes =>
          val pMessage = kryo fromBytes bytes
          pMessage match {
            case seed : GameSeed =>
              screenResources.beforeProcess.invoke {
                gameScreen.select()
                val opp = new RemoteOpponent(gameResources, this, seed.name, owner, owner, seed)
                setGame(new RemoteGameScreenContext(gameScreen, opp))
              }
            case ChatMessage(chatMsg) => logMsg(chatMsg)
            case ProxyAsk(msg, id) =>
                msg match {
                  case _ : AskOpponentInfo =>
                    messageQueue.clear()
                    proxyMessage(ProxyAnswer(id,
                      OpponentInfo(user, gameResources.playerChoices(owner).map(_.houseId), screenResources.storage.checksum)))
                  case _ => log.debug("unknown msg " + msg)
                }
            case ProxyAnswer(answerId, msg) =>
              pending(answerId).success(msg)
            case _ =>
              log.debug("message " + pMessage)
              messageQueue put pMessage
              log.debug("message " + pMessage + " enqueued")
          }
        }
      case MessageType.RequestFailed =>
        logMsg("request failed")
    }
  }

  def setGame(context : RemoteGameScreenContext) = {
    currentGame foreach (_.gameInit.releaseLocks())
    currentGame = Some(context)
  }

  def send(message : Message) = {
    val header = message.header.copy(length = message.body.map(_.length) getOrElse 0)
    out.write((header.toStr + "\n").getBytes(StandardCharsets.UTF_8))
    message.body foreach out.write
    out.flush()
    log.debug("send " + header + " " + message.body)
  }

  def proxify(m : Any) : Message = {
    val bytes : Array[Byte] = kryo.toBytesWithClass(m)
    Message(Header(MessageType.Proxy), Some(bytes))
  }

  def proxyMessage(m : Any) = send(proxify(m))

  def proxyAsk(m : Any) : Promise[Any] = {
    val msg = ProxyAsk(m)
    send(proxify(msg))
    val prom = Promise[Any]()
    pending.put(msg.id, prom : Promise[Any])
    prom
  }

  def getMessage() : Option[Message] = {
    Header.fromStr(readLine()) map { header =>
      if (header.length > 0) {
        val bytes = new Array[Byte](header.length)
        in read bytes
        Message(header, Some(bytes))
      } else Message(header, None)
    }
  }

  def readLine() : String = {
    var result = ""
    var c = in.read()
    while (c != -1 && c.toChar != '\n') {
      result += c.toChar
      c = in.read()
    }
    result
  }

  def release() {
    running = false
    socket.close()
    logMsg("Disconnected")
    setPlayerList(Nil)
    messageQueue put GameSeed.PoisonPill
    messageQueue.clear()
    pending.clear()
  }

}
