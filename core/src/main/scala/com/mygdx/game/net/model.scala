package com.mygdx.game.net

import java.nio.charset.StandardCharsets
import java.util.Base64

object MessageType extends Enumeration {
  val Welcome, Name, RequestDuel, RequestFailed, NewGame, Proxy, ExitDuel, ListPlayers = Value
}

object Header {
  val currentId = new java.util.concurrent.atomic.AtomicInteger

  def fromStr(header : String) = {
    header.split(";").toList.map(_.toInt) match {
      case messageTypeId :: length :: msgId :: answerId :: _ => Some(Header(MessageType(messageTypeId), length, msgId, answerId))
      case _ => None
    }
  }
}
case class Header(messageType : MessageType.Value, length : Int = 0, id : Int = Header.currentId.incrementAndGet, answerId : Int = 0) {
  def toStr = messageType.id + ";" + length + ";" + id + ";" + answerId
}
case class Message(header : Header, body : Option[Array[Byte]] = None)

case class ChatMessage(s : String)

case class ProxyAsk(msg : Any, id : Int = Header.currentId.incrementAndGet())
case class ProxyAnswer(id: Int, msg : Any)
class AskOpponentInfo
case class OpponentInfo(name : String, houses : List[Int], checksum : String)


object PlayerStatus extends Enumeration {
  val OnHold, Duelling = Value
}
object PlayerInfo {
  def decode(p : String) = {
    val name :: status :: id :: _ = p.split(":").toList
    PlayerInfo(new String(Base64.getDecoder() decode name, StandardCharsets.UTF_8), id, PlayerStatus(status.toInt))
  }
}
case class PlayerInfo(name : String, id : String, status : PlayerStatus.Value) {
  override def toString() = {
    status match {
      case PlayerStatus.Duelling => name + "(" + status + ")"
      case _ => name
    }
  }
}
