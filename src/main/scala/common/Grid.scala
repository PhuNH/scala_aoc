package common

trait Grid[T] {
  def length: Int
  def width: Int

  def getAtPos(pos: Coords): T
  def setAtPos(pos: Coords, value: T): Unit
}
