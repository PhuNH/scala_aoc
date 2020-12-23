package y2020.w4

import common.Day

class Day23 extends Day("463528179", "389125467") {
  private val labels: IndexedSeq[Int] = inputs(0).map(_.toString.toInt)

  private def convertIndex(i: Int, length: Int): Int = if (i < 0) i+length else if (i >= length) i-length else i

  // works with part one but takes forever with part two
  private def crab(times: Int, labels: IndexedSeq[Int], mode: Int = 1): String = {
    val length = labels.length
    var current = 0
    var buf = labels
    var pickup = IndexedSeq.empty[Int]
    var t = 0
    while (t < times) {
      var expectedDest = buf(current) - 1
      if (expectedDest < 1) expectedDest = length
      val pickupStart = convertIndex(current+1, length)
      val pickupEnd = convertIndex(current+3, length)
      if (current > pickupEnd) current -= (pickupEnd+1)

      if (pickupStart < pickupEnd) {
        pickup = buf.slice(pickupStart, pickupEnd+1)
        buf = buf.slice(0, pickupStart) ++ buf.slice(pickupEnd+1, length)
      } else {
        pickup = buf.slice(pickupStart, length) ++ buf.slice(0, pickupEnd+1)
        buf = buf.slice(pickupEnd+1, pickupStart)
      }
      while (pickup.contains(expectedDest)) {
        expectedDest -= 1
        if (expectedDest < 1) expectedDest = length
      }
      val destIndex = buf.indexOf(expectedDest)
      if (destIndex < current) current += 3
      buf = buf.slice(0, destIndex+1) ++ pickup ++ buf.slice(destIndex+1, length-3)

      current = convertIndex(current+1, length)
//      println(s"$i ${buf.slice(0, 100).mkString(" ")}")
      t += 1
    }
    val oneIndex = buf.indexOf(1)
    if (mode == 1) (buf.slice(oneIndex+1,length) ++ buf.slice(0,oneIndex)).mkString("")
    else (buf(convertIndex(oneIndex+1, length)).toLong * buf(convertIndex(oneIndex+2, length))).toString
  }

  // using a hint from reddit, which I think I also did similarly at some point before
  private def useLinkedArray(times: Int, input: IndexedSeq[Int], mode: Int = 1): String = {
    val length = if (mode == 1) 9 else 1000000
    val indexOrder = input.map(_ - 1)
    val firstNine = new Array[Int](9)
    (0 until 8).foreach(i => firstNine(indexOrder(i)) = indexOrder(i+1))
    val linkedArray =
      if (mode == 1) {
        firstNine(indexOrder(length-1)) = indexOrder(0)
        firstNine
      } else {
        firstNine(indexOrder(8)) = 9
        firstNine ++ (10 until 1000000) :+ indexOrder(0)
      }

    var current = input(0) - 1
    var t = 0
    while (t < times) {
      var pickups = IndexedSeq.empty[Int]
      var expectedDest = current
      if (expectedDest < 1) expectedDest = length
      val indexToPickUp = linkedArray(current)

      var nextIndex = indexToPickUp
      (1 to 2).foreach(_ => {
        pickups = pickups :+ (nextIndex+1)
        nextIndex = linkedArray(nextIndex)
      })
      pickups = pickups :+ (nextIndex+1)
      val lastIndexToPickUp = nextIndex
      nextIndex = linkedArray(nextIndex)
      linkedArray(current) = nextIndex

      while (pickups.contains(expectedDest)) {
        expectedDest -= 1
        if (expectedDest < 1) expectedDest = length
      }
      val destIndex = expectedDest-1
      val afterDestIndex = linkedArray(destIndex)
      linkedArray(destIndex) = indexToPickUp
      linkedArray(lastIndexToPickUp) = afterDestIndex

      current = linkedArray(current)
      t += 1
    }
    val oneIndex = 0
    if (mode == 1) {
      var str = ""
      var nextIndex = linkedArray(oneIndex)
      while (nextIndex != oneIndex) {
        str += (nextIndex+1)
        nextIndex = linkedArray(nextIndex)
      }
      str
    } else {
      val nextIndex = linkedArray(oneIndex)
      val nextNextIndex = linkedArray(nextIndex)
      ((nextIndex+1).toLong * (nextNextIndex+1)).toString
    }
  }

  def one: Unit = println(useLinkedArray(100, labels))

  def two: Unit = println(useLinkedArray(10000000, labels, 2))
}