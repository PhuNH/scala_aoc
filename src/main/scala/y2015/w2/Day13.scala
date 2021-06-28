package y2015.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day13 extends Day(inputPath(2015, 13)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def processData(data: Array[String]): Map[String, Map[String, Int]] = {
    data.map(l => {
      val parts = l.split(' ')
      (parts(0), (parts(10).init, parts(3).toInt * (if (parts(2) == "lose") -1 else 1)))
    }).groupMapReduce(_._1)(ssi => Map(ssi._2))((si1, si2) => si1.concat(si2))
  }

  private val relations = processData(data)

  private def tryRec(relations: Map[String, Map[String, Int]],
                     positions: List[String],
                     changeSum: Int): Array[(List[String], Int)] =
    if (positions.length == relations.size-1) {
      val last = relations.keys.find(!positions.contains(_)).get
      val changes = relations(positions.head)(last) + relations(last)(positions.head) +
        relations(positions.last)(last) + relations(last)(positions.last)
      Array((last :: positions, changeSum + changes))
    } else relations.keys.toArray.filter(!positions.contains(_)).flatMap(n =>
      tryRec(relations, n :: positions, changeSum + relations(positions.head)(n) + relations(n)(positions.head)))

  def one: Int = relations.keys.map(n => tryRec(relations, List(n), 0).maxBy(_._2)).maxBy(_._2)._2

  def two: Int = {
    val relationsWithYou: Map[String, Map[String, Int]] =
      relations.map(km => (km._1, km._2.updated("Y", 0))).updated("Y", relations.keys.map((_, 0)).toMap)
    relations.keys.map(n => tryRec(relationsWithYou, List(n), 0).maxBy(_._2)).maxBy(_._2)._2
  }
}