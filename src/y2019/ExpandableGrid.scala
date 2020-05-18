package y2019

import common.Direction

import scala.collection.mutable.ArrayBuffer

class ExpandableGrid {
  private val data = ArrayBuffer.fill(ExpandableGrid.InitialSize)(
    ArrayBuffer.fill(ExpandableGrid.InitialSize)(ExpandableGrid.Unknown))

  def height: Int = data.size
  def width: Int = data(0).size

  def getAtPos(pos: TwoD): Int = data(pos.v)(pos.h)

  def setAtPos(pos: TwoD, value: Int): Unit = data(pos.v)(pos.h) = value

  def expand(direction: Direction.Value,
             expandingSize: Int = ExpandableGrid.ExpandingSize): Unit = direction match {
    case Direction.North =>
      val toBePrepended = ArrayBuffer.fill(expandingSize)(ArrayBuffer.fill(width)(ExpandableGrid.Unknown))
      data.prependAll(toBePrepended)
    case Direction.South =>
      val toBeAppended = ArrayBuffer.fill(expandingSize)(ArrayBuffer.fill(width)(ExpandableGrid.Unknown))
      data.appendAll(toBeAppended)
    case Direction.West =>
      data.map(_.prependAll(Array.fill(expandingSize)(ExpandableGrid.Unknown)))
    case Direction.East =>
      data.map(_.appendAll(Array.fill(expandingSize)(ExpandableGrid.Unknown)))
  }

  override def toString: String = data.map(row => row.mkString(" ")).mkString("\n")
}

object ExpandableGrid {
  val Unknown: Int = -1

  val InitialSize = 9
  val InitialIndex = 4
  val ExpandingSize = 5

  def apply() = new ExpandableGrid
}