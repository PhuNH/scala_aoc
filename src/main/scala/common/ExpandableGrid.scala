package common

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class ExpandableGrid[T] extends Grid[T] {
  case class Init()
  implicit def initToT(i: Init): T = ExpandableGrid.Unknown.asInstanceOf[T]

  private var data = ArrayBuffer.fill(ExpandableGrid.InitialSize)(
    ArrayBuffer.fill[T](ExpandableGrid.InitialSize)(Init()))

  override def length: Int = data.size
  override def width: Int = data(0).size

  override def getAtPos(pos: Coords): T = data(pos.v)(pos.h)
  override def setAtPos(pos: Coords, value: T): Unit = data(pos.v)(pos.h) = value

  def expand(direction: Direction.Value,
             expandingSize: Int = ExpandableGrid.ExpandingSize)(implicit tag: ClassTag[T]): Unit = direction match {
    case Direction.North =>
      val toBePrepended = ArrayBuffer.fill(expandingSize)(ArrayBuffer.fill[T](width)(Init()))
      data.prependAll(toBePrepended)
    case Direction.South =>
      val toBeAppended = ArrayBuffer.fill(expandingSize)(ArrayBuffer.fill[T](width)(Init()))
      data.appendAll(toBeAppended)
    case Direction.West =>
      data.map(_.prependAll(Array.fill[T](expandingSize)(Init())))
    case Direction.East =>
      data.map(_.appendAll(Array.fill[T](expandingSize)(Init())))
  }

  def setData(newData: ArrayBuffer[ArrayBuffer[T]]): Unit = data = newData

  override def toString: String = data.map(row => row.mkString(" ")).mkString("\n")

  def toCharString: String = data.map(row => row.map(_.toString).mkString(" ")).mkString("\n")
}

object ExpandableGrid {
  val Unknown: Int = -1

  val InitialSize = 9
  val InitialIndex = 4
  val ExpandingSize = 5

  def apply[T]() = new ExpandableGrid[T]
}