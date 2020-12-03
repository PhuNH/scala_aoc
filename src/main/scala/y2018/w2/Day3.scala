package y2018.w2

import common.Day
import common.Utils._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Day3 extends Day(inputPath(2018, 3)) {
  private val fabric: ArrayBuffer[ArrayBuffer[List[Int]]] = ArrayBuffer.fill(1000)(ArrayBuffer.fill(1000)(List[Int]()))

  private val claimStrings = using(Source.fromResource(inputs(0)))(_.getLines().toArray.map(_.split('@')(1).trim))

  private def handleClaim(claim: String, index: Int): Unit = {
    val startAndSizes = claim.split(':').map(_.trim)
    val start = startAndSizes(0).split(',').map(_.toInt) // [x, y]
    val sizes = startAndSizes(1).split('x').map(_.toInt) // [w, h]
    if (start(0) + sizes(0) > fabric(0).length)
      fabric.map(_.appendAll(Array.fill(start(0)+sizes(0)-fabric(0).length)(List())))
    if (start(1) + sizes(1) > fabric.length)
      fabric.appendAll(ArrayBuffer.fill(start(1)+sizes(1)-fabric.length)(ArrayBuffer.fill(fabric(0).length)(List())))
    for (i <- Range(start(1), start(1)+sizes(1)))
      for (j <- Range(start(0), start(0)+sizes(0)))
        fabric(i)(j) = fabric(i)(j).appended(index)
  }

  for (i <- claimStrings.indices)
    handleClaim(claimStrings(i), i)

  def one: Int = fabric.map(_.count(_.length > 1)).sum

  def two: Int = {
    val onces = fabric.flatten.filter(_.length == 1).map(_.head).toSet
    onces.find(index => {
      val startAndSizes = claimStrings(index).split(':').map(_.trim)
      val start = startAndSizes(0).split(',').map(_.toInt) // [x, y]
      val sizes = startAndSizes(1).split('x').map(_.toInt) // [w, h]
      val claimedFabric = fabric.slice(start(1), start(1)+sizes(1)).map(_.slice(start(0), start(0)+sizes(0)))
      claimedFabric.forall(_.forall(_.length == 1))
    }).get + 1
  }
}
