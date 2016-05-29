package com.mygdx.game.net

import java.io._
import java.net._
import java.util.Base64

import collection._
import scala.concurrent.Promise
import priv.util.Utils.thread
import com.mygdx.game.{RemoteGameScreenContext, Screens}
import priv.sp._

class NetClient(host : String, port : Int, user : String,
                screens : Screens,
                log : String => Unit,
                setPlayerList : List[String] => Unit) {

  import screens._

  val socket  = new Socket(host, port)
  val out     = socket.getOutputStream()
  val in      = socket.getInputStream()
  val pout    = new PrintWriter(out, true)
  val reader  = new BufferedReader(new InputStreamReader(in))
  val pending = mutable.HashMap.empty[Int, Promise[Message]]
  val messageQueue = new java.util.concurrent.LinkedBlockingQueue[Any]
  def kryo    = GameKryoInstantiator.kryo

  var running = true
  thread("net client") {
    while (running) {
      getMessage() foreach { message =>
        if (message.header.answerId > 0) {
          pending(message.header.answerId).success(message) // TODO useless?
        } else {
          handle(message)
        }
      }
    }
    release()
  }

  def handle( message : Message ) = {
    message.header.messageType match {
      case MessageType.Welcome =>
        message.body foreach ( x => log(new String(x)) )
        send(Message(Header(MessageType.Name), Some(user.getBytes)))
      case MessageType.ListPlayers =>
        message.body foreach { bytes =>
          setPlayerList(new String(bytes).split(";").toList.map(p => new String(Base64.getDecoder() decode p)))
        }
      case MessageType.NewGame =>
        // a new game has been requested
        val seed = GameSeed.create(gameResources)
        // send the seed to the opponent
        proxyMessage(seed)
        gameScreen.select()
        val opp = new RemoteOpponent(gameResources, this, "remote", opponent, owner, seed )
        new RemoteGameScreenContext(gameScreen, opp)
      case MessageType.ExitDuel =>
        gameScreen.returnToStart()
      case MessageType.Proxy =>
        message.body foreach { bytes =>
          val pMessage = kryo fromBytes bytes
          pMessage match {
            case seed : GameSeed =>
              val opp = new RemoteOpponent(gameResources, this, "remote", owner, owner, seed )
              new RemoteGameScreenContext(gameScreen, opp)
            case _ =>
              messageQueue put pMessage
          }
        }
    }
  }

  def send(message : Message) = {
    val header = message.header.copy(length = message.body.map(_.length) getOrElse 0)
    pout.println(header.toStr)
    message.body foreach out.write
    out.flush()
  }

  def proxyMessage(m : Any) = {
    val bytes : Array[Byte] = kryo.toBytesWithClass(m)
    send(Message(Header(MessageType.Proxy), Some(bytes)))
  }

  def getMessage() : Option[Message] = {
    Header.fromStr(reader.readLine()) map { header =>
      if (header.length > 0) {
        val bytes = new Array[Byte](header.length)
        in read bytes
        Message(header, Some(bytes))
      } else Message(header, None)
    }
  }

  def release() {
    running = false
    socket.close()
    log("Disconnected")
    setPlayerList(Nil)
    messageQueue.clear()
    pending.clear()
  }

}
