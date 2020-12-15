package common

object Direction extends Enumeration {
  val North: Direction.Value = Value(1, "^")
  val South: Direction.Value = Value(2, "v")
  val West: Direction.Value = Value(3, "<")
  val East: Direction.Value = Value(4, ">")

  def valueToVector(direction: Direction.Value): Coords = direction match {
    case North => Coords(-1, 0)
    case South => Coords(1, 0)
    case West => Coords(0, -1)
    case East => Coords(0, 1)
  }

  def leftFrom(direction: Direction.Value): Direction.Value = direction match {
    case North => West
    case South => East
    case West => South
    case East => North
  }

  def rightFrom(direction: Direction.Value): Direction.Value = direction match {
    case North => East
    case South => West
    case West => North
    case East => South
  }
}
