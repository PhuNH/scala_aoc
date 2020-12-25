package common

import scala.reflect.ClassTag

class FixedGrid[T](var data: Array[Array[T]])(implicit tag: ClassTag[T]) extends Grid[T] {
  override def length: Int = data.length
  override def width: Int = data(0).length

  override def getAtPos(pos: Coords): T = data(pos.v)(pos.h)
  override def setAtPos(pos: Coords, value: T): Unit = data(pos.v)(pos.h) = value

  def setData(newData: Array[Array[T]]): Unit = data = newData
}

object FixedGrid {
  def makeFrom[T: ClassTag](newData: Array[Array[T]]): FixedGrid[T] = new FixedGrid[T](newData)
}
