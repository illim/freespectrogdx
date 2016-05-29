package priv.sp.bot

import priv.sp._
import priv.sp.update._
import collection._
import scala.util.Random

trait CardStats {
  def getRandomCard(board: BoardView): Option[CardDesc]
  def update(reward: Float, cards: List[Card]): Unit
}

class DummyCardStats(val playerId: PlayerId, context: BotContext, knowledge: BotKnowledge) extends CardStats {

  def getRandomCard(board: BoardView): Option[CardDesc] = {
    val desc = board.p.desc.get
    val houseDescs = desc.houses
    val cards = houseDescs.foldLeft(Vector.empty[CardDesc]) { (acc, h) ⇒
      val houseState = board.p.houses(h.house.houseIndex)
      val cards = h.cards.filter { _.isAvailable(houseState) }
      acc ++ cards
    }
    val idx = Random.nextInt(cards.size + 1)
    if (idx >= cards.size) None
    else Some(cards(idx))
  }

  def update(reward: Float, cards: List[Card]) {}
}

/**
 * // bs magic number(i'm bored)
 * object CardStats {
 * val maybe = 0.7f
 * val givemeachance = 0.2f
 * val convinceditsbetter = 3
 * val hoursofpratice = 100
 * @inline def letsayhewaitforthis(card : Card, score : Float) = {
 * card.cost * score
 * }
 * }
 *
 * // stats for policy to be less random
 * class DumbCardStats(val playerId : PlayerId, context : BotContext, knowledge : BotKnowledge) extends CardStats  {
 * import CardStats._
 * import context._
 * val isHighBetter = playerId == botPlayerId
 *
 * case class CardStat(card : Card, isHypothetical : Boolean, var total : Float = 0f , var nbUsed : Int = 0){
 * val fact = if (isHypothetical) maybe else 1f
 * var score = 1f
 *
 * // retarded bs map total/nbUsed to R+
 * def refreshScore(){
 * score = letsayhewaitforthis(card, fact * (if (nbUsed == 0) givemeachance else if (total == 0) 0f else {
 * if (isHighBetter){
 * if (total <0) - nbUsed/ (nbUsed + total) else 1 + total / nbUsed
 * } else {
 * if (total <0) 1-(total / nbUsed) else nbUsed / (nbUsed + total)
 * }
 * }))
 * }
 *
 * final override def toString() = s"CardStat($total, $nbUsed, $score)"
 * }
 *
 * var stats = immutable.Map.empty[Card, CardStat]
 *
 * start.players(playerId).desc.get.houses.foreach{ h =>
 * h.cards.foreach{ cardDesc =>
 * val isHypothetical = (playerId != botPlayerId && cardDesc.card.cost < 9 && ! knowledge.knownCards.exists(_._1 == cardDesc.card))
 * stats += (cardDesc.card -> CardStat(cardDesc.card, isHypothetical))
 * }
 * }
 *
 * def getRandomCard(p : PlayerState) : Option[CardDesc] = {
 * val houseDescs = p.desc.get.houses
 * var cards = immutable.Map.empty[CardDesc, CardStat]
 * houseDescs.foreach{ h =>
 * val houseState = p.houses(h.house.houseIndex)
 * h.cards.foreach{ c =>
 * if (c.isAvailable(houseState)){
 * stats.get(c.card).foreach{ stat => // todo update list
 * stat.refreshScore()
 * cards += (c -> stat)
 * }
 * }
 * }
 * }
 * val cs = cards.size
 * val total = (0d /: cards){ (tot, c) =>
 * tot + c._2.score
 * }
 * val mean = total/cs
 * val filtereds = ((0d, List.empty[(CardDesc, Double)]) /: cards){ case (current @ (tot, acc), (c, stat)) =>
 * val newsc = if (stat.nbUsed < hoursofpratice) stat.score else math.pow(stat.score / mean, convinceditsbetter)
 * (tot + newsc, (c, newsc) :: acc)
 * }
 * //println("sig " + sig + ", discard " + (cs - cards.size))
 * val r = Random.nextFloat * filtereds._1
 * var res = Option.empty[CardDesc]
 * var acc = 0d
 * var ite = filtereds._2.iterator
 * while(res.isEmpty && ite.hasNext){
 * val c = ite.next
 * acc += c._2
 * if (acc > r){
 * res = Some(c._1)
 * }
 * }
 * res
 * }
 *
 * def update(reward : Float, cards : List[Card]){
 * //if (playerId == 0){    println("update " +playerId + ":"+ reward + " for " + cards)    }
 * cards.foreach{ c =>
 * stats.get(c).foreach{ stat =>
 * stat.total += reward
 * stat.nbUsed += 1
 * }
 * }
 * }
 * }
 */
/**
 *
 * import breeze.util._
 *
 * class StatMap [A] {
 *
 * case class Entry(rewards : Float, count : Int = 1) {
 * def add(reward : Float) = Entry(reward + rewards, count +1)
 * }
 *
 * class Stats {
 * private val index = Index[A]()
 * private var m = Vector(Vector.empty[Entry])
 *
 * private def add(i : Int, j : Int, reward : Float) {
 * if (m.size < i) {
 * m = m ++ Vector.fill(i - m.size)(Vector.empty[Entry])
 * }
 * var row = m(i)
 * if (row.size < j) {
 * row = row ++ Vector.fill(j - m.size)(Entry(0f))
 * }
 * val value = row(j).add(reward)
 * m = m.updated(i, row.updated(j, value))
 * }
 *
 * def add(xs : Vector[A], reward : Float) {
 * for(x <- xs) {
 * val i = index.index(x)
 * for(y <- xs) {
 * val j = index.index(y)
 * if (i != j) {
 * add(i, j, reward)
 * add(j, i, reward)
 * }
 * }
 * }
 * }
 *
 * }
 *
 * }
 */
