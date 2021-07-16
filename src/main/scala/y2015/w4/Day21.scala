package y2015.w4

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day21 extends Day(inputPath(2015, 21)) {
  case class Item(cost: Int, dam: Int = 0, ar: Int = 0)

  case class Subject(hp: Int, dam: Int, ar: Int, cost: Int = 0) {
    def this(t: IndexedSeq[Int]) = this(t(0), t(1), t(2))

    def takeDam(other: Subject): Subject = {
      val takenDam = if (ar >= other.dam) 1 else other.dam - ar
      this.copy(hp = hp - takenDam)
    }

    def use(items: IndexedSeq[Item]): Subject =
      this.copy(dam = dam + items.map(_.dam).sum, ar = ar + items.map(_.ar).sum, cost = items.map(_.cost).sum)
  }

  private val data: IndexedSeq[Int] = using(Source.fromResource(inputs(0)))(
    _.getLines().toIndexedSeq.map(_.split(':')(1).tail.toInt))
  private val boss: Subject = new Subject(data)
  private val player: Subject = Subject(100, 0, 0)
  private val weapons = IndexedSeq(Item(8, 4), Item(10, 5), Item(25, 6), Item(40, 7), Item(74, 8))
  private val armors =
    IndexedSeq(Item(0), Item(13, ar = 1), Item(31, ar = 2), Item(53, ar = 3), Item(75, ar = 4), Item(102, ar = 5))
  private val rings: IndexedSeq[Item] =
    IndexedSeq(Item(0), Item(0),
      Item(25, 1), Item(50, 2), Item(100, 3),
      Item(20, ar = 1), Item(40, ar = 2), Item(80, ar = 3))

  @tailrec
  private def fight(player: Subject, boss: Subject, turn: Int): Int = {
    if (player.hp <= 0) 0
    else if (boss.hp <= 0) 1
    else if (turn % 2 == 0) fight(player, boss.takeDam(player), turn+1)
    else fight(player.takeDam(boss), boss, turn+1)
  }

  private val allSuites: IndexedSeq[Subject] = for {
    w <- weapons
    a <- armors
    r <- rings.combinations(2)
  } yield player.use(w +: a +: r)

  def one: Int = allSuites.filter(fight(_, boss, 0) > 0).map(_.cost).min

  def two: Int = allSuites.filter(fight(_, boss, 0) == 0).map(_.cost).max
}