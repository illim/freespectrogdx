package priv.sp

package object house {
  import priv.sp.update._

  def lowestLife(s1: SlotUpdate, s2: SlotUpdate) = if (s2.get.life < s1.get.life) s2 else s1
  def highestLife(s1: SlotUpdate, s2: SlotUpdate) = if (s2.get.life > s1.get.life) s2 else s1
  def strongest(s1: SlotUpdate, s2: SlotUpdate) = if (s2.get.attack > s1.get.attack) s2 else s1

  def nearestEmptySlot(selected: Int, player: PlayerUpdate): Option[Int] = {
    val slots = player.slots.slots
    val dists = player.value.slotList collect { case n if slots(n).value.isEmpty ⇒ (n, math.abs(n - selected)) }
    if (dists.isEmpty) None
    else Some(dists.minBy(_._2)._1)
  }

  def nearestSlotOpposed(selected: Int, player: PlayerUpdate, opposed: Boolean = true): Option[Int] = {
    val slots = player.slots.slots
    val otherSlots = player.otherPlayer.getSlots
    val dists = player.value.slotList collect {
      case n if n != selected
        && slots(n).value.isEmpty
        && otherSlots.isDefinedAt(n) == opposed ⇒ (n, math.abs(n - selected))
    }
    if (dists.isEmpty) None
    else Some(dists.minBy(_._2)._1)
  }

  @inline def nonSpecial(p: PlayerId, state: GameState): List[Int] = listSlotWhere(p, state)(_.houseIndex < 4)
  @inline def special(p: PlayerId, state: GameState): List[Int] = listSlotWhere(p, state)(_.houseIndex == 4)

  def listSlotWhere(p: PlayerId, state: GameState)(f : Card => Boolean): List[Int] = {
    state.players(p).slots.foldLeft(List.empty[Int]) {
      case (acc, (i, s)) ⇒
        if (f(s.card)) i :: acc else acc
    }
  }
}
