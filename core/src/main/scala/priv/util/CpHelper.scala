package priv.util

import oscar.cp.modeling._
import oscar.cp.core._
import scala.util.Random

object CpHelper {

  // todo why _.randomValue not work?
  def getRandom(x: CPIntVar) = {
    val ind = Random.nextInt(x.size)
    x.toArray.apply(ind)
  }

}

import CpHelper._

trait CpHelper {

  implicit def solver: CPSolver

  def contains(x: Int, l: Traversable[CPIntVar]) = {
    l.foldLeft(CPBoolVar(false)) { (acc, e) ⇒
      acc || (e === x)
    }
  }

  def notContains(x: Int, l: Traversable[CPIntVar]) = {
    l.foldLeft(CPBoolVar(true)) { (acc, e) ⇒
      acc && (e !== x)
    }
  }

  val MAXRETRY = 3
  def softRun(vars: Seq[_ <: CPIntVar], timeLimit: Int = Int.MaxValue) = {
    def isCompleted = allBounds(vars)

    if (timeLimit != Int.MaxValue) {
      start(nSols = 1, timeLimit = timeLimit)
    } else {
      var failed = true
      var i = 0
      while (failed && i < MAXRETRY) {
        start(nSols = 1, timeLimit = 1 + i)
        failed = solver.isFailed || !isCompleted
        i += 1
      }
      if (solver.isFailed) {
        println("last retry")
        start(nSols = 1, timeLimit = Int.MaxValue)
      }
    }
  }
}
