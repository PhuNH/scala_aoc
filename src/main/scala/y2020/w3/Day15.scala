package y2020.w3

import common.Day

class Day15 extends Day("5,1,9,18,13,8,0") {
  private val startingNumbers = inputs(0).split(',').map(_.toInt)
  private val startingNumbersWithLastTurns: Map[Int, Int] = startingNumbers.init.zip(1 until startingNumbers.length).toMap

  private def play(startingNumbersWithLastTurns: Map[Int, Int], turnCount: Int): Int = {
    var numbersWithLastTurns = startingNumbersWithLastTurns
    var turn = startingNumbers.length
    var prevNum = startingNumbers.last

    while (turn < turnCount) {
      turn += 1
      if (!numbersWithLastTurns.contains(prevNum)) {
        numbersWithLastTurns += ((prevNum, turn-1))
        prevNum = 0
      } else {
        val thisNum = turn-1 - numbersWithLastTurns(prevNum)
        numbersWithLastTurns += ((prevNum, turn-1))
        prevNum = thisNum
      }
    }
    prevNum
  }

  def one: Int = play(startingNumbersWithLastTurns, 2020)

  def two: Int = play(startingNumbersWithLastTurns, 30000000)
}