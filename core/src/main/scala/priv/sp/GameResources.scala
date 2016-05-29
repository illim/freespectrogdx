package priv.sp

import java.util.concurrent._
import java.net._
import java.nio.channels._
import priv.util.Utils._

// bullcrap :)

object GameResources {
  type Closable = { def close() }
}
import GameResources._

class GameResources {
  val sp           = new SpWorld
  val aiExecutor   = Executors.newSingleThreadExecutor
  val gameExecutor = Executors.newSingleThreadExecutor
  val multi        = new Resources
  val serverSocket = multi(new ClosableOne[ServerSocketChannel])
  val clientSocket = multi(new ClosableOne[Socket])
  var ended        = false

  import java.net._
  import scala.collection.JavaConversions._
  val networkInterfaces = NetworkInterface.getNetworkInterfaces.toList filter { _.isUp() }
  var networkInterface = networkInterfaces.headOption
  def getAddr(port: Int = 0) = networkInterface.flatMap { n ⇒
    n.getInetAddresses.toList.headOption map { a ⇒
      new InetSocketAddress(a, port)
    }
  }.get

  val base = sp.houses.others ++ sp.houses.sinist
  val baseEnemy = base ++ sp.houses.bs
  var playerChoices: List[List[House]] = List(base, baseEnemy)
  def resolveChoices = playerChoices map { o ⇒
    val l = if (o.isEmpty) sp.houses.special else o
    l(scala.util.Random.nextInt(l.size))
  }

  def dispose() {
    ended = true
    println("releasing resources")
    multi.release()
    aiExecutor.shutdown()
    gameExecutor.shutdown()
  }
}

class Resources {
  private val resources = new ConcurrentLinkedQueue[Resource]
  def apply[A <: Resource](x: A): A = {
    resources.add(x)
    x
  }

  def release() { iterate(resources.iterator)(_.release()) }
}

trait Resource {
  def release()
}

abstract class One[A] extends Resource {
  private var x = Option.empty[A]
  def apply(v: ⇒ A): A = {
    release()
    val a = v
    x = Some(a)
    a
  }
  def release(v: A)
  def release() {
    x foreach release _
  }
}

class ClosableOne[A <: Closable] extends One[A] {
  def release(v: A) = v.close()
}
