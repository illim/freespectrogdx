package priv.sp.bot

import scala.collection._
import priv.sp._
import priv.sp.update._

// simple life delta
class LifeHeuris(context: BotContext, val settings: Settings) {
  import context._
  val name = "Rushomon" // not a real rusher
  var start = null.asInstanceOf[HeurisValue]

  def init(st: GameState) { start = new HeurisValue(st) }
  def apply(state: GameState): Float = {
    math.max(0f, (40f + ((new HeurisValue(state)).lifeDelta - start.lifeDelta).toFloat) / 100f)
  }

  class HeurisValue(state: GameState) {
    private val bot = state.players(botPlayerId)
    private val human = state.players(humanId)

    val lifeDelta = bot.life - human.life
  }

}

/**
 * a ratio using mana ratio when there's less pressure on life
 * the theory is that the more there's pressure on life, the less there's freedom to maximize mana or board
 */
class LifeManaHeuris(context: BotContext, val settings: Settings) {
  import context._
  val name = "lifemanaratio" // not a real rusher
  var start = null.asInstanceOf[HeurisValue]
  var pressure = 0f
  val manaFactor = 0.7f

  def init(st: GameState) {
    start = new HeurisValue(st)
    pressure = math.min(1, math.min(start.bot.life, start.human.life) / 60)
  }
  def apply(state: GameState): Float = {
    math.max(0f, (1 + (new HeurisValue(state).value - start.value))/ 2f)
  }

  class HeurisValue(state: GameState) {
    val bot = state.players(botPlayerId)
    val human = state.players(humanId)

    val lifeDelta = bot.life - human.life
    val manaDelta = manaRatio(bot) - manaRatio(human)
    val boardDelta = boardRatio(bot) - boardRatio(human)
    val value = ((1 - pressure) * ( math.max(0f, 40f + lifeDelta) / 100f )
    + pressure * (pressure * manaDelta + (1- pressure) * boardDelta))

    def manaRatio(p : PlayerState) = (p.houses.take(4).map(h => h.mana / 12f).sum + p.houses(4).mana / 8f) / 5f
    def boardRatio(p : PlayerState) = p.slots.map{ case (i, s) => s.life.toFloat / s.card.life }.sum / 6f
  }

}
