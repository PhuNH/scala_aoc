package common

case class Coords(var v: Int, var h: Int) extends Ordered[Coords] {
  def update(v: Int = this.v, h: Int = this.h): Unit = {
    this.v = v
    this.h = h
  }
  def updateWith(that: Coords): Unit = update(that.v, that.h)

  def +=(that: Coords): Unit = update(v + that.v, h + that.h)
  def *=(num: Int): Unit = update(v * num, h * num)

  def north: Coords = this.copy(v = this.v - 1)
  def south: Coords = this.copy(v = this.v + 1)
  def west: Coords = this.copy(h = this.h - 1)
  def east: Coords = this.copy(h = this.h + 1)
  def adjacent: Array[Coords] = Array(north, west, east, south)

  def swap: Coords = Coords(h, v)

  def +(that: Coords): Coords = Coords(v + that.v, h + that.h)
  def *(num: Int): Coords = Coords(v * num, h * num)
  def *(that: Coords): Coords = Coords(v * that.v, h * that.h)

  def nw: Coords = this + Coords(-1, -1)
  def ne: Coords = this + Coords(-1, 1)
  def sw: Coords = this + Coords(1, -1)
  def se: Coords = this + Coords(1, 1)
  def diagonal: Array[Coords] = Array(nw, ne, sw, se)

  def selfMhtDist: Int = math.abs(v) + math.abs(h)

  def mhtDistWith(that: Coords): Int = math.abs(v - that.v) + math.abs(h - that.h)

  override def clone: Coords = new Coords(v, h)

  override def compare(that: Coords): Int = {
    val vComp = Integer.compare(v, that.v)
    if (vComp != 0) vComp
    else Integer.compare(h, that.h)
  }
}

object Coords {
  val Unknown: Int = -1

  def unknown: Coords = Coords(Unknown, Unknown)
}
