package priv.sp.bot

import scala.util.Random
import priv.sp._
import priv.sp.update._
import priv.util._
import collection._

// for random effects, the simulator add a max of 2 additional evaluations
// todo weight the heuris result considering the random factors from choices and effects, not only the number of turns
// card stats + warmup ?

object BoundedBot2AI extends BotTree {
  type TreeLabel = Node
  val currentNodeId = new java.util.concurrent.atomic.AtomicLong()

  def logNode(node: Node, observer: BotObserver, children: Traversable[String]) = {
    val name = node.commandOpt.map(_.toString).getOrElse("")

    s"""{"name" : "$name","stats" : "${observer.getNodeStat(node).statString}",
       |"children" : [${children.mkString(",\n")}]}""".stripMargin
  }
  def logRun(policyRun: PolicyRun) = {
    def logCommand(command: Option[Command], child: String) = {
      val name = command.map(_.toString).getOrElse("")

      s"""{"name" : "$name", "stats" : "","children" : [$child]}""".stripMargin
    }
    policyRun.commands.foldLeft("") { (child, command) ⇒
      logCommand(command, child)
    }
  }
  def dump(tree: Tree, observer: BotObserver): String = {
    val l = tree.label
    val children: Traversable[String] = if (tree.subforest.isEmpty) {
      l.policyRuns.map { run ⇒ logRun(run) }.filter(_.nonEmpty)
    } else tree.subforest.toList.map(c ⇒ dump(c, observer))
    logNode(l, observer, children)
  }
}

import BoundedBot2AI._
import BoundedBot2AI.Tree

case class Node(
    from: GameState,
    transition: Transition,
    playerStats: List[PlayerStats],
    end : Option[PlayerId],
    getChildren: Node ⇒ Stream[Tree],
    commandOpt: Option[Command] = None,
    parent: Option[Node] = None) {

  val playerId = transition.playerId
  val id = currentNodeId.incrementAndGet()
  var policyRuns = Vector.empty[PolicyRun]
  def isLeaf = end.isDefined
  lazy val children = getChildren(this)
}

class PolicyRun {
  var commands = List.empty[Option[Command]] // inverted list
}

class BoundedBot2AI(simulator: BotSimulator) {
  import simulator.context._

  def context = simulator.context

  val observer = new BotObserver(context, simulator.knowledge)
  val choices = observer.choices

  private def treeNode(node: Node) = {
    new Tree(node)
  }

  def execute() = {
    val node = treeNode(Node(start, WaitPlayer(botPlayerId), simulator.updater.stats, simulator.updater.ended, getChildren _))
    val loc = node.loc
    val (timeSpent, nbIterations) = Bot.loopWhile(settings) {
      treePolicy(loc) match {
        case Some(selected) ⇒
          defaultPolicy(selected); true
        case None ⇒ false
      }
    }
    val result = node.subforest.foldLeft(Option.empty[TreeLabel]) {
      case (acc, childTree) ⇒
        val childStat = observer getNodeStat childTree.label
        println(childStat.statString)
        acc match {
          case Some(node) ⇒
            if (observer.getNodeStat(node).getAvgReward < childStat.getAvgReward)
              Some(childTree.label)
            else acc
          case None ⇒ Some(childTree.label)
        }
    }
    (node, result.flatMap { node ⇒
      val nodeStat = observer getNodeStat node
      println(s"ai spent $timeSpent, numSim : ${nodeStat.numSim}, ${nodeStat.nbWin}/${nodeStat.nbLoss},  ${nbIterations} iterations")
      node.commandOpt
    })
  }

  val defaultPolicyMaxTurn = 10
  def defaultPolicy(loc: TreeP) = {
    val tree = loc.tree
    var nbStep = 0
    val node = tree.label
    var state = node.from
    var end = node.end
    var player = node.playerId
    simulator.updater.resetStats()
    val cardUsage = new observer.CardUsage
    val policyRun = new PolicyRun
    while (nbStep < defaultPolicyMaxTurn && end.isEmpty) {
      val nextCommand = choices.getRandomMove(state, player)
      nextCommand.foreach { c ⇒ cardUsage.cardUsed(player, c.card) }
      policyRun.commands = nextCommand :: policyRun.commands
      val (gameState, transition) = simulator.simulateCommand(state, player, nextCommand)
      state = gameState
      player = transition.playerId
      nbStep += 1
      end = simulator.updater.ended
    }
    node.policyRuns = node.policyRuns :+ policyRun
    observer.updateStats(loc, state, end, simulator.updater.stats, cardUsage)
    end.isDefined
  }

  def getFirstChild(loc: TreeP): Option[TreeP] = {
    val label = loc.tree.label
    val children = label.children // compute the children
    if (children.headOption.isDefined) {
      if (loc.tree.subforest.isEmpty) {
        loc.tree.subforest = children // attach the children
      }
      Some(loc.child)
    } else None
  }

  val maxDepth = 2
  def isLeaf(loc: TreeP) = (loc.depth == maxDepth) || loc.tree.label.isLeaf

  final def treePolicy(start: TreeP): Option[TreeP] = {
    var loc = start
    var end = false
    def endWith(t: TreeP) {
      loc = t
      end = true
    }

    while (!end) {
      select(loc)
      if (isLeaf(loc)) {
        endWith(loc)
      } else {
        getFirstChild(loc) match {
          case None        ⇒ endWith(loc)
          case Some(child) ⇒ loc = child
        }
      }
    }

    if (loc ne start) Some(loc) else None
  }

  private def select(t: TreeP) = {
    t.parent foreach { p ⇒
      var best = (t.tree, t.pos._1)
      val isFairOnly = p.tree.label.playerId == humanId
      while (t.gotoNext()) {
        if (observer.select(t.tree.label, best._1.label, isFairOnly)) {
          best = (t.tree, t.pos._1)
        }
      }
      t gobackto best._2
    }
  }

  val maxRandomChild = 4
  def getChildren(node: Node): Stream[Tree] = {
    def getChild(commandOpt: Option[Command]) = {
      val (state, outTransition) = simulator.simulateCommand(node.from, node.playerId, commandOpt)
      val randWidth = simulator.updater.randLogs.width
      simulator.updater.resetRand()
      val playerStats = simulator.updater.stats.map(_.copy())
      (randWidth, treeNode(Node(state, outTransition, playerStats, simulator.updater.ended, getChildren _, commandOpt, Some(node))))
    }

    if (node.isLeaf) {
      Stream.Empty
    } else {
      val commandChoices = choices.getNexts(node.from, node.transition.playerId)

      new Stream.Cons(None, commandChoices.map(Some(_))).flatMap { cmd ⇒
        val (randWidth, t) = getChild(cmd)
        if (randWidth > 1) {
          t :: List.fill(math.min(randWidth, maxRandomChild)) { getChild(cmd)._2 }
        } else List(t)
      }
    }
  }

}

class NodeStat(val node: Node, parentStat: Option[NodeStat]) {
  var numSim = 1f
  var nbWin = 0
  var nbLoss = 0
  var rewards = 0f

  def getAvgReward: Float = rewards / numSim
  def getUct: Float = parentStat.map { p ⇒
    (getAvgReward / 5f) + math.sqrt(2 * math.log(p.numSim) / numSim).floatValue // HACK to try to have reward in [0, 1], it goes rarely up to 10
  }.sum
  def getFair = parentStat.map { p ⇒
    math.sqrt(2 * math.log(p.numSim) / numSim).floatValue
  }.sum
  def statString = s"Node(${node.commandOpt} : $getUct, avgRwd=$getAvgReward, nSim=$numSim)"
}

class BotObserver(context: BotContext, knowledge: BotKnowledge) {
  import context._

  private val nodeStats = mutable.LongMap.empty[NodeStat]
  def getNodeStat(node: Node): NodeStat = {
    nodeStats.get(node.id) match {
      case Some(n) ⇒ n
      case None ⇒
        val n = new NodeStat(node, node.parent.map(getNodeStat))
        nodeStats.put(node.id, n)
        n
    }
  }

  val cardStats = playerIds.map { p ⇒ new DummyCardStats(p, context, knowledge) }
  val choices = new Choices(cardStats, settings)
  val heuris = new LifeManaHeuris(context, settings)
  heuris.init(start)

  def select(node1: Node, node2: Node, isFairOnly: Boolean) = {
    val stat1 = getNodeStat(node1)
    val stat2 = getNodeStat(node2)
    (isFairOnly && stat1.getFair > stat2.getFair) || stat1.getUct > stat2.getUct
  }

  def updateStats(loc: TreeP, st: GameState, end: Option[PlayerId], playerStats: List[PlayerStats], cardUsage: CardUsage) = {
    val node = loc.tree.label
    val nodeStat = getNodeStat(node)
    val stats = playerIds map { i ⇒
      playerStats(i) + node.playerStats(i)
    }
    val h = heuris(st) //, stats, loc.depth)
    val reward = end.map { p ⇒ if (p == botPlayerId) 1f else -1f }.getOrElse(h)

    nodeStat.numSim += 1
    nodeStat.rewards += reward

    loc backPropagate { n ⇒
      val parentStat = getNodeStat(n)
      end foreach { p ⇒
        if (p == botPlayerId) { parentStat.nbWin += 1 } else { parentStat.nbLoss += 1 }
      }
      parentStat.numSim += 1
      parentStat.rewards += reward
    }
    updateCardStats(cardUsage, reward)
    reward
  }

  def updateCardStats(cardUsage: CardUsage, reward: Float) {
    cardStats(botPlayerId).update(reward, cardUsage.botCards)
    cardStats(humanId).update(reward, cardUsage.oppCards)
  }

  class CardUsage {
    var botCards = List.empty[Card]
    var oppCards = List.empty[Card]
    def cardUsed(player: PlayerId, card: Card) {
      if (player == context.humanId) {
        oppCards = card :: oppCards
      } else {
        botCards = card :: botCards
      }
    }
  }
}

class BoundedBot2(val botPlayerId: PlayerId, val gameDesc: GameDesc, val spHouses: Houses, val settings: Settings = new Settings) {
  val knowledge = new BotKnowledge(gameDesc, spHouses, botPlayerId)

  def executeAI(start: GameState) = {
    debugExecuteAI(start)._1._2
  }

  def debugExecuteAI(start: GameState) = {
    val st = knowledge.k ripDescReader start
    val context = BotContext(botPlayerId, st, settings)
    val simulator = new BotSimulator(knowledge, context)
    val ai = new BoundedBot2AI(simulator)
    (ai.execute(), ai.observer)
  }
}
