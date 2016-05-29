package priv.sp.bot

import priv.sp._
import priv.sp.update._

case class Settings(
    var logging: Boolean = true,
    var duration: Int = 4000,
    var simBlockedSlotWeight: Float = 0.7f,
    var boostFactor: Float = 2.5f, //3,
    var rewardDefaultWeightFactor: Float = 0.04f,
    var attackBonus: Float = 0.15f, // 1/20
    var slotOccupationBonus: Float = 0.4f,
    var manaPow: Float = 1.2f, // 1.7
    var specialManaPow: Float = 1.4f, // 2
    var killFactor: Float = 1f,
    var powerFactor: Float = 1f,
    var noEffectMalus: Float = 0f,
    var oppPowerFactor: Float = 1f) {

  def getCostPowMana(m: Float, houseIndex: Int) = {
    if (houseIndex == 4) math.pow(m, specialManaPow) else math.pow(m, manaPow)
  }

  def hpManaRatio(slot: SlotState): Double = (slot.card.life / (0.5 + getCostPowMana(slot.card.cost, slot.card.houseIndex)))
}
