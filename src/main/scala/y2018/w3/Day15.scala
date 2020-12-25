package y2018.w3

import common.{Coords, Day, FixedGrid, Grid}
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day15 extends Day(inputPath(2018, 15), testPath(2018, 15, 1),
  testPath(2018,15,2), testPath(2018,15,3),
  testPath(2018,15,4), testPath(2018,15,5),
  testPath(2018,15,6), testPath(2018,15,7),
  testPath(2018,15,8), testPath(2018,15,9)) {
  private val Goblin = 'G'.toInt
  private val Elf = 'E'.toInt
  private val kinds = Array(Goblin, Elf)

  class Creature(kind: Int, val coords: Coords, var power: Int = 3, var hp: Int = 200) {
    def isAlive: Boolean = hp > 0
    def isGoblin: Boolean = kind == Goblin

    def findAdjacentEnemy(goblins: IndexedSeq[Creature], elves: IndexedSeq[Creature]): Option[Creature] = {
      val enemies = (if (isGoblin) elves else goblins).sortWith((c1, c2) =>
        if (c1.hp == c2.hp) c1.coords < c2.coords else c1.hp < c2.hp)
      enemies.find(e => coords.adjacent.contains(e.coords))
    }

    @tailrec
    private def getNextStepRecursive(targets: Set[Coords], distanceGrid: FixedGrid[(Int, Coords)],
                                     isGood: Coords => Boolean,
                                     stepQueue: Array[(Int, Coords, Coords)], // length, start, current
                                     minTargets: Array[(Coords, Coords)], // start, target
                                     minDist: Int): Option[Coords] = {
      if (stepQueue.isEmpty) {
        if (minTargets.isEmpty) None
        else Some(minTargets.minBy(_._2)._1)
      } else {
        val popped = stepQueue(0)
        if (minDist != 0 && popped._1+1 > minDist) return Some(minTargets.minBy(_._2)._1)

        var curMinDist = minDist
        var curMinTargets = minTargets
        if (targets.contains(popped._3)) {
          if (curMinDist == 0) {
            curMinDist = popped._1+1
            curMinTargets = Array((popped._2, popped._3))
          } else if (curMinDist == popped._1+1) curMinTargets = curMinTargets :+ (popped._2, popped._3)
        }
        distanceGrid.setAtPos(popped._3, (popped._1 + 1, popped._2))
        val nextStepQueue = stepQueue.tail ++ popped._3.adjacent.filter(c =>
          distanceGrid.getAtPos(c)._1 == -2 && isGood(c)).map(c => {
          distanceGrid.setAtPos(c, (-1, Coords.unknown))
          (popped._1+1, popped._2, c)
        })
        getNextStepRecursive(targets, distanceGrid, isGood, nextStepQueue, curMinTargets, curMinDist)
      }
    }

    def getNextStep(goblins: IndexedSeq[Creature], elves: IndexedSeq[Creature],
                    map: FixedGrid[Int]): Option[Coords] = {
      def isGood(coords: Coords): Boolean = map.getAtPos(coords) == '.'

      val enemies = if (isGoblin) elves else goblins
      val targets = enemies.flatMap(_.coords.adjacent).toSet.filter(isGood)
      if (targets.isEmpty) None
      else {
        val distanceData = Array.fill(map.length)(Array.fill(map.width)((-2, Coords.unknown)))
        val distanceGrid = FixedGrid.makeFrom(distanceData)
        distanceGrid.setAtPos(coords, (0, coords))
        val stepQueue = coords.adjacent.filter(c => isGood(c)).map(c => (0, c, c))
        getNextStepRecursive(targets, distanceGrid, isGood, stepQueue, Array.empty[(Coords, Coords)], 0)
      }
    }

    def moveAttack(goblins: IndexedSeq[Creature], elves: IndexedSeq[Creature],
                   map: FixedGrid[Int]): Unit = getNextStep(goblins, elves, map).foreach(nextCoords =>  {
      map.setAtPos(coords, '.')
      coords.updateWith(nextCoords)
      map.setAtPos(nextCoords, kind)
      val enemy = findAdjacentEnemy(goblins, elves)
      if (enemy.nonEmpty) attack(enemy.get, map)
    })

    def attack(that: Creature, map: FixedGrid[Int]): Unit = {
      that.hp -= power
      if (that.hp <= 0) map.setAtPos(that.coords, '.')
    }

    override def clone: Creature = new Creature(kind, coords.clone)
  }

  // 1: 27730 - 2: 36334 - 3: 39514 - 4: 27755 - 5: 28944 - 6: 18740
  // reddit: 7: 215168
  // reddit: 8: 248848
  // reddit: 9: 195774
  private val data: Array[Array[Int]] = using(Source.fromResource(inputs(0)))(
    _.getLines().toArray.map(_.toCharArray.map(_.toInt)))
  private val creatures: IndexedSeq[Creature] = for {
    i <- data.indices
    j <- data(0).indices
    if kinds.contains(data(i)(j))
  } yield new Creature(data(i)(j), Coords(i, j))

  private def round(map: FixedGrid[Int], creatures: IndexedSeq[Creature], rounds: Int, elfCnt: Int): (Int, Boolean) = {
//    println(s"Round $count:")
//    println(map.data.map(row => row.map(_.toChar).mkString("")).mkString("\n"))

    val aliveCreatures = creatures.filter(_.isAlive).sortBy(_.coords)
    var (goblins, elves) = aliveCreatures.partition(_.isGoblin)

    for (creature <- aliveCreatures) if (creature.isAlive) {
      if (goblins.isEmpty || elves.isEmpty) return (rounds * (goblins ++ elves).map(_.hp).sum, elves.length == elfCnt)
      creature.findAdjacentEnemy(goblins, elves) match {
        case None => creature.moveAttack(goblins, elves, map)
        case Some(enemy) => creature.attack(enemy, map)
      }
      if (creature.isGoblin) elves = elves.filter(_.isAlive)
      else goblins = goblins.filter(_.isAlive)
    }
    round(map, aliveCreatures, rounds+1, elfCnt)
  }

  private def cloneMap: FixedGrid[Int] = {
    val clonedData = Array.fill(data.length)(new Array[Int](data(0).length))
    Grid.cloneGrid(data, clonedData)
    FixedGrid.makeFrom(clonedData)
  }

  private def cloneCreatures: IndexedSeq[Creature] = creatures.map(c => c.clone)

  def one: Int = round(cloneMap, cloneCreatures, 0, creatures.count(!_.isGoblin))._1

  // 1: 4988 - 3: 31284 - 4: 3478 - 5: 6474 - 6: 1140
  def two: Int = {
    var power = 4
    val (goblins, elves) = creatures.partition(_.isGoblin)
    do {
      val buffedElves = elves.map(c => new Creature(Elf, c.coords.clone, power))
      val clonedGoblins = goblins.map(c => new Creature(Goblin, c.coords.clone))
      val (outcome, result) = round(cloneMap, clonedGoblins ++ buffedElves, 0, elves.length)
      if (result) return outcome
      power += 1
    } while (true)
    0
  }
}