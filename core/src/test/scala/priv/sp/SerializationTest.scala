package priv.sp

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import com.mygdx.game.net.GameKryoInstantiator

@RunWith(classOf[JUnitRunner])
class SerializationTest extends FlatSpec with Matchers {
  val kryo = GameKryoInstantiator.kryo

  it should "serialize card" in {
    val card = HouseSingleton.entomologist.giantAnt
    val bytes : Array[Byte] = kryo.toBytesWithClass(card)
    val c = kryo.fromBytes(bytes)
    println(c + ":" + bytes.length)
    c should equal(card)
    (c eq card) should be(true)
  }
}