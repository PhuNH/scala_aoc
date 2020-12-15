package y2018.w3

import common.{Day, Grid}

class Day11 extends Day("7403") {
  type Grid = Array[Array[Int]]

  private val gridSerialNumber = inputs(0).toInt
  private val grid: Grid = Array.fill(300)(Array.fill(300)(0))
  for (y <- 1 to 300)
    for (x <- 1 to 300)
      grid(y-1)(x-1) = powerLevel(x, y)

  private def powerLevel(x: Int, y: Int): Int = {
    val rackId = x + 10
    val pLevel = (rackId * y + gridSerialNumber) * rackId / 100 % 10 - 5
    pLevel
  }

  private def powerLevelSum(v: Int, h: Int, size: Int, prevSums: Grid): Int = {
    val colSum = (0 until size).map(dv => grid(v+dv)(h+size-1)).sum
    val rowSum = (0 until size-1).map(dh => grid(v+size-1)(h+dh)).sum
    val sum = colSum + rowSum + prevSums(v)(h)
    prevSums(v)(h) = sum
    sum
  }

  private def find(sizes: Seq[Int]): String = {
    val prevSums = new Array[Array[Int]](300)
    Grid.cloneGrid(grid, prevSums)
    var sizeOfMax = 1
    var vOfMax = 0
    var hOfMax = 0
    var max = 0

    var max3 = 0
    var vOfMax3 = 0
    var hOfMax3 = 0

    for (v <- 0 until 300) for (h <- 0 until 300)
      if (grid(v)(h) > max) {
        vOfMax = v
        hOfMax = h
        max = grid(v)(h)
      }

    for (size <- sizes.tail) for (v <- 0 to 300-size) for (h <- 0 to 300-size) {
      val sum = powerLevelSum(v, h, size, prevSums)
      if (sum > max) {
        vOfMax = v
        hOfMax = h
        sizeOfMax = size
        max = sum
      }
      if (size == 3) {
        if (sum > max3) {
          vOfMax3 = v
          hOfMax3 = h
          max3 = sum
        }
      }
    }

    s"${hOfMax3+1},${vOfMax3+1}\n${hOfMax+1},${vOfMax+1},$sizeOfMax"
  }

  def one: Unit = {}

  def two: Unit = println(find(1 to 300))
}