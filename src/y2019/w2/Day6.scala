package y2019.w2

import common.Day
import common.Utils._

import scala.collection.mutable
import scala.io.Source

class Day6 extends Day(inputPath(2019, 6)) {
  private val m = mutable.Map[String, List[String]]()

  using(Source.fromFile(inputs.head))(_.getLines().foreach(l => {
    val relation = l.split(')')
    val orbit = relation(1)
    val center = relation(0)
    if (!m.contains(center))
      m.addOne((center, List()))
    val newTail = center :: m(center)
    m.update(orbit, newTail)
    if (m.contains(orbit))
      m.filter(_._2.contains(orbit)).foreach(b =>
        m.update(b._1, b._2 ::: newTail))
  }))

  def one: Int = m.values.map(_.length).sum

  def two: Int = {
    val brYou = m("YOU")
    val brSan = m("SAN")
    brYou.diff(brSan).length + brSan.diff(brYou).length
  }
}