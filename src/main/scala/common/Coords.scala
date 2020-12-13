package common

case class Coords(var v: Int, var h: Int) {
  def update(v: Int = this.v, h: Int = this.h): Unit = {
    this.v = v
    this.h = h
  }

  def increase(dV: Int = 0, dH: Int = 0): Unit = update(v + dV, h + dH)

  def copyFrom(another: Coords): Unit = update(another.v, another.h)

  def north: Coords = this.copy(v = this.v - 1)

  def south: Coords = this.copy(v = this.v + 1)

  def west: Coords = this.copy(h = this.h - 1)

  def east: Coords = this.copy(h = this.h + 1)

  def +(that: Coords): Coords = Coords(v+that.v, h+that.h)

  def *(num: Int): Coords = Coords(v*num, h*num)
}

object Coords {
  val Unknown: Int = -1

  def unknown: Coords = Coords(Unknown, Unknown)
}
