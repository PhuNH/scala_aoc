package y2019.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day3 extends Day(inputPath(2019, 3)) {
  private object SectString {
    def unapply(sect: String): Option[(Char, Int)] = {
      if (sect.length < 2) None
      else Some((sect(0), sect.tail.toInt))
    }
  }

  private def generateSteps(path: List[String]): List[((Int, Int), Int)] = {
    def genSectSteps(start: ((Int, Int), Int), sect: String): List[((Int, Int), Int)] = sect match {
      case SectString('U', n) => (1 to n).map(s => ((start._1._1, start._1._2 + s), start._2 + s)).toList
      case SectString('D', n) => (1 to n).map(s => ((start._1._1, start._1._2 - s), start._2 + s)).toList
      case SectString('L', n) => (1 to n).map(s => ((start._1._1 - s, start._1._2), start._2 + s)).toList
      case SectString('R', n) => (1 to n).map(s => ((start._1._1 + s, start._1._2), start._2 + s)).toList
    }

    @scala.annotation.tailrec
    def recGen(start: ((Int, Int), Int),
               path: List[String],
               steps: List[((Int, Int), Int)]): List[((Int, Int), Int)] = path match {
      case h :: t =>
        val newSteps = genSectSteps(start, h)
        val newStart = newSteps.last
        recGen(newStart, t, steps ++ newSteps)
      case _ => steps
    }
    recGen(((0, 0), 0), path, List())
  }

  private val paths: List[List[((Int, Int), Int)]] = using(Source.fromFile(inputs.head))(
    _.getLines().map(l => generateSteps(l.split(",").toList)).toList
  )

  private def findCrossPoints(paths: List[List[((Int, Int), Int)]]): List[((Int, Int), Int)] = {
    val firstPath = paths.head
    val secondPath = paths(1)
    val firstPointCoords = firstPath.map(_._1)
    val secondPointCoords = secondPath.map(_._1)
    val crossPointCoords = firstPointCoords.intersect(secondPointCoords).distinct
    crossPointCoords.map(c => {
      val firstMinDist = firstPath.filter(_._1 == c).map(_._2).min
      val secondMinDist = secondPath.filter(_._1 == c).map(_._2).min
      (c, firstMinDist + secondMinDist)
    })
    //  val firstCrossPoints = firstPath.filter(p => secondPointCoords.contains(p._1)).groupBy(_._1)
    //    .map { case (k,v) => (k,v.map(_._2).min) }.toList
    //  firstCrossPoints.map(p => {
    //    val p2Dist = secondPath.filter(_._1 == p._1).map(_._2).min
    //    (p._1, p._2 + p2Dist)
    //  })
    // why does this take forever?
  }

  private val crossPoints: List[((Int, Int), Int)] = findCrossPoints(paths)

  private def calMhtDist(point: (Int, Int)): Int = Math.abs(point._1) + Math.abs(point._2)

  def one: Int = crossPoints.map(p => calMhtDist(p._1)).min

  def two: Int = crossPoints.map(_._2).min
}

// 6 | 30
//val lines = Iterator("R8,U5,L5,D3", "U7,R6,D4,L4")
// 159 | 610
//val lines = Iterator("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")
// 135 | 410
//val lines = Iterator("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

//val paths = lines.map(l => generateSteps(l.split(",").toList))
//val firstPath = paths.next()
//val secondPath = paths.next()
//val secondPointCoords = secondPath.map(_._1)
//val firstCrossPoints = firstPath.filter(p => secondPointCoords.contains(p._1)).groupBy(_._1)
//  .map { case (k,v) => (k,v.map(_._2).min) }.toList
//firstCrossPoints.map(p => {
//  val p2Dist = secondPath.filter(_._1 == p._1).map(_._2).min
//  (p._1, p._2 + p2Dist)
//})

//findCrossPoints(paths).map(_._2).min
//findCrossPoints(paths).map(p => calMhtDist(p._1)).min
