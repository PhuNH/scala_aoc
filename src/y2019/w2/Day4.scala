package y2019.w2

import common.Day

class Day4 extends Day("172930-683082") {
  @scala.annotation.tailrec
  private def checkAdjacency(l: List[Char]): Boolean = l match {
    case hd :: hd2 :: tl =>
      if (hd == hd2) true
      else checkAdjacency(hd2 :: tl)
    case _ => false
  }

  @scala.annotation.tailrec
  private def checkIncrease(l: List[Char]): Boolean = l match {
    case hd :: hd2 :: tl =>
      if (hd > hd2) false
      else checkIncrease(hd2 :: tl)
    case _ => true
  }

  private def checkAdjacency2(l: String): Boolean = {
    for (i <- 0 until l.length - 1) {
      if (i == 0) {
        if (l.charAt(i) == l.charAt(i+1) && l.charAt(i) != l.charAt(i+2))
          return true
      } else if (i == l.length - 2) {
        if (l.charAt(i) == l.charAt(i+1) && l.charAt(i) != l.charAt(i-1))
          return true
      } else {
        if (l.charAt(i) == l.charAt(i+1) && l.charAt(i) != l.charAt(i+2) && l.charAt(i) != l.charAt(i-1))
          return true
      }
    }
    false
  }

  private val bounds = inputs.head.split('-').map(_.toInt)

  def one: Int = (for {
    i <- bounds(0) to bounds(1)
    l = i.toString.toList
    if checkIncrease(l) && checkAdjacency(l)
  } yield i).length

  def two: Int = (for {
    i <- bounds(0) to bounds(1)
    s = i.toString
    if checkIncrease(s.toList) && checkAdjacency2(s)
  } yield i).length
}
