package common

trait Grid[T] {
  def length: Int
  def width: Int

  def getAtPos(pos: Coords): T
  def setAtPos(pos: Coords, value: T): Unit
}

object Grid {
  def cloneGrid[T](from: Array[Array[T]], to: Array[Array[T]]): Unit = for (i <- to.indices) to(i) = from(i).clone()
}
