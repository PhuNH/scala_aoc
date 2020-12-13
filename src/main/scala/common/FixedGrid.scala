package common

import scala.reflect.ClassTag

class FixedGrid[T](l: Int, w: Int)(implicit tag: ClassTag[T]) extends Grid[T] {
  private var data: Array[Array[T]] = Array.fill(l)(new Array[T](w))

  override def length: Int = l
  override def width: Int = w

  override def getAtPos(pos: Coords): T = data(pos.v)(pos.h)
  override def setAtPos(pos: Coords, value: T): Unit = data(pos.v)(pos.h) = value

  def setData(newData: Array[Array[T]]): Unit = data = newData
}

object FixedGrid {
  def makeFrom[T: ClassTag](newData: Array[Array[T]]): FixedGrid[T] = {
    val newFixedGrid = new FixedGrid[T](newData.length, newData(0).length)
    newFixedGrid.setData(newData)
    newFixedGrid
  }
}
