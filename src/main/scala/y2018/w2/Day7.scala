package y2018.w2

import common.Day
import common.Utils._

import scala.io.Source

class Day7 extends Day(inputPath(2018, 7), testPath(2018, 7, 1)) {
  private val instructions: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val parentsAndChildren = instructions.map(_.split(' ')).map(i => (i(1)(0), i(7)(0)))
  private val steps = (parentsAndChildren.map(_._1) ++ parentsAndChildren.map(_._2)).toSet
  private val fullSteps = steps.map(s => (s, parentsAndChildren.filter(_._2 == s).map(_._1)))

  def one: Unit = {
    var res = ""
    var leftSteps = fullSteps
    while (res.length < fullSteps.size) {
      val next = leftSteps.filter(_._2.isEmpty).map(_._1).toArray.sorted.apply(0)
      leftSteps = leftSteps.filterNot(_._1 == next).map(t => (t._1, t._2.filterNot(_ == next)))
      res = res + next
    }
    println(res)
  }

  private def timeOfStep(step: Char): Int = 60 + step - 64

  private def freeWorker(workers: Array[(Char, Int)]): Int = {
    for (i <- workers.indices)
      if (workers(i)._1 == '0')
        return i
    -1
  }

  private def isAllFree(workers: Array[(Char, Int)]): Boolean = workers.forall(_._1 == '0')

  private def runWorkers(workers: Array[(Char, Int)]): Array[Char] = {
    var done = Array.emptyCharArray
    for (i <- workers.indices)
      if (workers(i)._1 != '0') {
        if (workers(i)._2 == 1) done = done :+ workers(i)._1
        workers(i) = (if (workers(i)._2 == 1) '0' else workers(i)._1, workers(i)._2-1)
      }
    done
  }

  def two: Int = {
    val workers = Array.fill(5)(('0', 0))
    var leftSteps = fullSteps
    var time = 0
    do {
      var wId = freeWorker(workers)
      var available = leftSteps.filter(_._2.isEmpty)
      while (wId != -1 && available.nonEmpty) {
        val next = available.map(_._1).toArray.sorted.apply(0)
        workers(wId) = (next, timeOfStep(next))
        leftSteps = leftSteps.filterNot(_._1 == next)
        wId = freeWorker(workers)
        available = leftSteps.filter(_._2.isEmpty)
      }
      val done = runWorkers(workers)
      if (done.nonEmpty) {
        done.foreach(s =>
          leftSteps = leftSteps.map(t => (t._1, t._2.filterNot(_ == s))))
      }
      time += 1
    } while (!isAllFree(workers) || leftSteps.nonEmpty)
    time
  }
}
