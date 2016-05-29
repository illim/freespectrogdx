package priv.sp.update

import priv.sp._
/**
 * object PlayerStats {
 * def basicKillValue(slot : SlotState) = slot.card.cost * (if (slot.card.houseIndex == 4) 1.5f else 1f)
 * def hpManaRatio(slot : SlotState) : Double = hpManaRatio(slot.card)
 * @inline def hpManaRatio(card : Creature): Double = (card.life / (0.5 + getCostPowMana(card.cost, card.houseIndex)))
 *
 * def getCostMana(m : Int, houseIndex : Int) = {
 * if (houseIndex == 4) m * 1.5 else m
 * }
 * def getCostPowMana(m : Int, houseIndex : Int) = {
 * if (houseIndex == 4) math.pow(m, 1.2) else math.pow(m, 1.4)
 * }
 * }
 */

case class PlayerStats(var nbSummon: Int = 0, var nbKill: Int = 0, var nbDead: Int = 0) {

  def reset() {
    nbSummon = 0
    nbKill = 0
    nbDead = 0
  }

  def addKill(slot: SlotState) {
    nbKill += 1
  }

  def +(stats: PlayerStats) = PlayerStats(
    nbSummon + stats.nbSummon,
    nbKill + stats.nbKill,
    nbDead + stats.nbDead)
}
