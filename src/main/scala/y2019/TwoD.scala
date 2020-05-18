package y2019

case class TwoD(var v: Int, var h: Int) {
  def update(v: Int = this.v, h: Int = this.h): Unit = {
    this.v = v
    this.h = h
  }

  def increase(dV: Int = 0, dH: Int = 0): Unit = update(v + dV, h + dH)

  def copyFrom(another: TwoD): Unit = update(another.v, another.h)

  def north: TwoD = this.copy(v = this.v - 1)

  def south: TwoD = this.copy(v = this.v + 1)

  def west: TwoD = this.copy(h = this.h - 1)

  def east: TwoD = this.copy(h = this.h + 1)
}

object TwoD {
  def unknown: TwoD = TwoD(ExpandableGrid.Unknown, ExpandableGrid.Unknown)
}
