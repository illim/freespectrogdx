package com.mygdx.game.net

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
case class NetPlayer(id : Int, name : String)
