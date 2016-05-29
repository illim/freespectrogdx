package priv.sp.bot

import collection._

trait BotTree {
  type TreeLabel

  class Tree(val label: TreeLabel, var subforest: Stream[Tree] = Stream.Empty) {
    def loc = new TreeP(this, None, 0)
  }

  // tree pointer
  class TreeP(var tree: Tree, val parent: Option[TreeP] = None, val depth: Int = 0) {
    var pos = (0, parent map (_.tree.subforest))
    var forestCache = Vector((tree, pos._2))

    def gotoNext() = {
      pos._2.exists {
        case head #:: tail ⇒
          tree = head
          pos = (pos._1 + 1, Some(tail))
          forestCache = forestCache :+ (tree, pos._2)
          true
        case _ ⇒
          false
      }
    }

    def gobackto(p: Int) {
      val cached = forestCache(p)
      pos = (p, cached._2)
      tree = cached._1
    }

    def hasChild = tree.subforest.nonEmpty
    def child = new TreeP(tree.subforest.head, Some(this), depth + 1)
    def backPropagate(f: TreeLabel ⇒ Unit) {
      parent foreach { p ⇒
        f(p.tree.label)
        p backPropagate f
      }
    }
  }

}
