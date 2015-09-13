package priv.util

import java.io._

object Utils {

  def memo[A, B](f : A => B) = {
    val m = collection.mutable.Map.empty[A, B]

    { k : A =>
      m.getOrElseUpdate(k, f(k))
    }
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

class TVar[A <: AnyRef](richLock: RichLock) {
  import richLock.lock

  private var holder = Option.empty[A]
  def set(a: A) {
    holder = Some(a)
    lock.synchronized {
      lock.notifyAll()
    }
  }

  def get(): Option[A] = {
    lock.synchronized {
      lock.wait()
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

trait ResourceCache[A, B] {
  import collection._

  val resources = mutable.Map.empty[A, B]

  def get(path: A): B = resources.getOrElseUpdate(path, {
    load(path)
  })
  def gets(path: A*): List[B] = path.map(get)(breakOut)
  def getOrElseUpdate[C <: B](path: A, create: A ⇒ C): C = resources.getOrElseUpdate(path, create(path)).asInstanceOf[C]
  def load(path: A): B
  def clean()
}

abstract class FieldUpdate[A](parent: Option[FieldUpdate[_]], getValue: ⇒ A) {
  var tid = 0
  var dirty = 0
  var value = getValue
  var valuedirty = 0 // if dirty is set by children value is out of sync
  val update = new ObservableFunc1Unit(setValue(_: A))

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

class ObservableFunc1Unit[A](f: A ⇒ Unit) extends Function[A, Unit] {
  private var inner = f

  def apply(x: A) = inner(x)

  def after(g: A ⇒ Unit) {
    val old = inner
    inner = { x: A ⇒
      old(x)
      g(x)
    }
  }

  def before(g: A ⇒ Unit) {
    val old = inner
    inner = { x: A ⇒
      g(x)
      old(x)
    }
  }
}

class ObservableFunc1[A, B](f: A ⇒ B) extends Function[A, B] {
  private var inner = f

  def apply(x: A) = inner(x)

  def after(g: A ⇒ Unit) {
    val old = inner
    inner = { x: A ⇒
      val res = old(x)
      g(x)
      res
    }
  }

  def before(g: A ⇒ Unit) {
    val old = inner
    inner = { x: A ⇒
      g(x)
      old(x)
    }
  }
}

class InterceptableFunc1[A](f: A ⇒ A) extends Function[A, A] {
  private var inner = f

  def apply(x: A) = inner(x)

  def intercept(g: A ⇒ A) {
    val old = inner
    inner = { x: A ⇒
      g(old(x))
    }
  }
}
