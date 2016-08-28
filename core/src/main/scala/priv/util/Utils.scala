package priv.util

import java.io._
import java.net.URL
import java.nio.channels.Channels
import java.security.MessageDigest
import java.util.zip.ZipInputStream

import com.badlogic.gdx.Gdx

object Utils {

  def memo[A, B](f : A => B) = {
    val m = collection.mutable.Map.empty[A, B]

    { k : A =>
      m.getOrElseUpdate(k, f(k))
    }
  }

  def getChecksum() : String = {
    val currentJavaJarFile = new File(this.getClass.getProtectionDomain().getCodeSource().getLocation().getPath())
    val filepath = currentJavaJarFile.getAbsolutePath()
    try {
      val md = MessageDigest.getInstance("SHA-256")
      val fis = new FileInputStream(filepath)
      val dataBytes = new Array[Byte](1024)
      var n = fis read dataBytes

      while ( n != -1 ) {
        md.update(dataBytes, 0, n)
        n = fis read dataBytes
      }

      new String(md.digest())
    } catch { case e : Exception =>
      e.printStackTrace()
      "0"
    }
  }

  def download(url : URL, name : String, target : String): Unit = {
    val channel = Channels.newChannel(url.openStream())
    val out = new FileOutputStream(target)
    out.getChannel().transferFrom(channel, 0, Long.MaxValue)
  }

  val BUFFER = 2048
  def unzip(file : String, targetDir : String): Unit = {
    val fis = new FileInputStream(file)
    val zis = new ZipInputStream(new BufferedInputStream(fis))
    var entry = zis.getNextEntry
    while(entry != null) {
      var data = new Array[Byte](BUFFER)
      val target = new File(targetDir + File.separator + entry.getName())
      if (entry.getName().endsWith("/")) {
        target.mkdirs()
      } else {
        if (! target.getParentFile.exists()) {
          target.getParentFile.mkdirs()
        }
        val fos = new FileOutputStream(target)
        val dest = new BufferedOutputStream(fos, BUFFER)
        var count = zis.read(data, 0, BUFFER)
        while (count != -1) {
          dest.write(data, 0, count)
          count = zis.read(data, 0, BUFFER)
        }
        dest.flush()
        dest.close()
      }
      entry = zis.getNextEntry
    }
    zis.close()
  }

  def toBytes(o: AnyRef) = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    try {
      out.writeObject(o)
      bos.toByteArray()
    } finally {
      out.close()
      bos.close()
    }
  }

  def fromBytes(arr: Array[Byte]) = {
    val bis = new ByteArrayInputStream(arr);
    val in = new ObjectInputStream(bis);
    try {
      in.readObject()
    } finally {
      bis.close()
      in.close()
    }
  }

  def floatRand[N](n: N)(implicit num: Numeric[N]) = {
    val fl = scala.util.Random.nextFloat
    if (fl == 0) fl else num.toFloat(n) * math.abs(1 / fl)
  }

  def thread(name: String)(f: ⇒ Unit) = new Thread(runnable(f), name).start()
  def runnable(f: ⇒ Unit) = new Runnable() {
    def run() = {
      try {
        f
      } catch {
        case t: Throwable ⇒
          t.printStackTrace
      }
    }
  }

  def iterate[A](ite: java.util.Iterator[A])(f: A ⇒ Unit) {
    while (ite.hasNext) {
      f(ite.next)
    }
  }
}

class TVar[A <: AnyRef](val richLock: RichLock) {
  import richLock.lock

  @volatile private var holder = Option.empty[A]
  def set(a: A) {
    holder = Some(a)
    lock.synchronized {
      lock.notifyAll()
    }
  }

  def get(): Option[A] = {
    if (holder.isEmpty) {
      lock.synchronized {
        lock.wait()
      }
    }
    holder
  }
}

// some stupid lock that cant be waited on twice
// TODO clean this crap
class RichLock {
  import java.util.concurrent.atomic.AtomicBoolean
  @volatile var released = false
  @volatile var acquired = new AtomicBoolean
  val lock = new Object

  def release() { // this can be very dangerous if it is released during ai(TODO make it safer)
    lock.synchronized {
      released = true
      lock.notifyAll()
    }
  }

  // return none when surrendering for example
  def waitFor[A <: AnyRef](f: TVar[A] ⇒ Unit): Option[A] = {
    val t = new TVar[A](this)
    f(t)
    lock.synchronized {
      if (released) None else t.get()
    }
  }

  def waitLock(f: AnyRef ⇒ Unit) {
    f(lock)
    lockWait()
  }

  def lockWait() {
    lock.synchronized {
      if (!released && acquired.compareAndSet(false, true)) {
        lock.wait()
        acquired.compareAndSet(true, false)
      }
    }
  }
}

abstract class FieldUpdate[A](parent: Option[FieldUpdate[_]], getValue: ⇒ A) {
  var tid = 0
  var dirty = 0
  var value = getValue
  var valuedirty = 0 // if dirty is set by children value is out of sync
  val update = new FuncDecorator1(setValue(_: A))

  def initNewUpdate(value: A): this.type = {
    write(value)
    tid += 1
    this
  }

  def reinit(): this.type = {
    if (!isInited) {
      dirty = 0
      valuedirty = 0
      value = getValue
      tid = parent.get.tid
    }
    this
  }

  def write(v: A) {
    setDirty()
    update(v)
  }

  // f is a whole rebuild of the value
  def updated(f: ⇒ A): A = {
    if (valuedirty != dirty) {
      valuedirty = dirty
      update(f)
    }
    value
  }

  def setValue(v: A) { value = v }

  def isInited = tid == parent.get.tid
  def isDirty = parent.exists(_.tid == tid && dirty > 0)
  def invalidate() { tid = parent.get.tid - 1 }
  def setDirty() {
    parent.foreach { p ⇒
      p.setDirty()
      dirty += 1
    }
  }

}

object FuncDecorators {

  def decorate[A](f : () => A) : FuncDecorator0[A] = {
    f match {
      case o : FuncDecorator0[A] => o
      case _ => new FuncDecorator0[A](f)
    }
  }

  def decorate[A, B](f : A => B) : FuncDecorator1[A, B] = {
    f match {
      case o : FuncDecorator1[A, B] => o
      case _ => new FuncDecorator1[A, B](f)
    }
  }
}

class FuncDecorator1[A, B](f: A ⇒ B) extends Function[A, B] {
  private var inner = f

  def apply(x: A) = inner(x)

  def update(f : (A => B) => (A => B)) =  {
    val old = inner
    inner = f(old)
    this
  }


  def modifyResult(g: B ⇒ B) = {
    val old = inner
    inner = { x: A ⇒
      g(old(x))
    }
    this
  }

  def after(g: A ⇒ Unit) = {
    val old = inner
    inner = { x: A ⇒
      val res = old(x)
      g(x)
      res
    }
    this
  }

  def after2(g : (A, B) => Unit) = {
    val old = inner
    inner = { x: A ⇒
      val res = old(x)
      g(x, res)
      res
    }
    this
  }

  def before(g: A ⇒ Unit) = {
    val old = inner
    inner = { x: A ⇒
      g(x)
      old(x)
    }
    this
  }
}


class FuncDecorator0[A](f: () ⇒ A) extends Function0[A] {
  private var inner = f

  def apply() = inner()

  def update(f : (() => A) => (() => A)) =  {
    val old = inner
    inner = f(old)
    this
  }

  def modifyResult(g: A ⇒ A) {
    val old = inner
    inner = { () =>
      g(old())
    }
  }
}

class Log(x : AnyRef) {
  val name = x.getClass.getSimpleName
  def debug(s : String) = Gdx.app.debug(name, ts + ">" + s)
  def info(s : String)  = Gdx.app.log(name, ts + ">" + s)
  def error(s : String) = Gdx.app.error(name, ts + ">" + s)
  private def ts = Thread.currentThread().getName
}
