package priv.sp.house

import priv.sp._

class DudeMancer {

  val Dude = House("Dudes", List(
    new Creature("Bred", Attack(4), 17),
    new Creature("Two P", Attack(4), 27),
    new Creature("Axl", Attack(5), 26),
    new Creature("Bill Bull", Attack(7), 31),
    new Creature("El Gado", Attack(8), 46),
    new Creature("Andore", Attack(9), 52),
    new Creature("Rolento", Attack(10), 65),
    new Creature("Edi E", Attack(10), 99)))

  Dude.initCards(Houses.basicCostFunc)
}
