package com.mygdx.game.net

import com.twitter.chill.{KryoPool, ScalaKryoInstantiator}
import _root_.priv.sp.{Card, House, HouseSingleton}
import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
            
object GameKryoInstantiator {
  val kryo = KryoPool.withByteArrayOutputStream(1, new GameKryoInstantiator)
}

class GameKryoInstantiator extends ScalaKryoInstantiator {
  override def newKryo() = {
    val k = super.newKryo()
    k.forSubclass[Card](new Serializer[Card] {
      override def write(kryo: Kryo, output: Output, card: Card): Unit = {
        kryo.writeObject(output, new Integer(card.id))
      }

      override def read(kryo: Kryo, input: Input, `type`: Class[Card]): Card = {
        val id = kryo.readObject(input, classOf[Integer])
        HouseSingleton.getCardById(id)
      }
    })
    k.forClass[House](new Serializer[House] {
      override def write(kryo: Kryo, output: Output, house: House): Unit = {
        kryo.writeObject(output, new Integer(house.houseId))
      }

      override def read(kryo: Kryo, input: Input, `type`: Class[House]): House = {
        val id = kryo.readObject(input, classOf[Integer])
        HouseSingleton.getHouseById(id)
      }
    })
    k
  }
}