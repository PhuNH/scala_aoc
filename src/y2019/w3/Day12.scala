package y2019.w3

import common.Day
import common.Utils._

import scala.collection.mutable
import scala.io.Source
import scala.math.signum

class Day12 extends Day(inputPath(2019, 12),
  testPath(2019, 12, 1), testPath(2019, 12, 2)) {

  case class Moon(position: Array[Int], velocity: Array[Int]) {
    private def arrayToString(array: Array[Int]): String = s"<${array.head}, ${array(1)}, ${array(2)}>"

    override def toString: String = s"pos=${arrayToString(position)}, vel=${arrayToString(velocity)}"

    override def equals(obj: Any): Boolean = obj match {
      case that: Moon => (position sameElements that.position) && (velocity sameElements that.velocity)
      case _ => false
    }

    override def hashCode(): Int =
      ((position: IndexedSeq[Int]).hashCode() + 41) * 41 + (velocity: IndexedSeq[Int]).hashCode()
  }

  case class State(moons: Array[Moon]) {
    override def toString: String = moons.mkString("\n")

    override def equals(obj: Any): Boolean = obj match {
      case that: State => moons sameElements that.moons
      case _ => false
    }

    override def hashCode(): Int = (moons: IndexedSeq[Moon]).hashCode()
  }

  private def initState(filePath: String): State = State(
    using(Source.fromFile(filePath))(source => {
      source.getLines().toArray.map(l => {
        val splitted = l.split("=, <>".toArray)
        Moon(Array(splitted(2).toInt, splitted(5).toInt, splitted(8).toInt), Array(0, 0, 0))
      })
    })
  )

  private def energy(state: State): Int = {
    state.moons.map(moon => {
      val pot = moon.position.map(Math.abs).sum
      val kin = moon.velocity.map(Math.abs).sum
      pot * kin
    }).sum
  }

  private def simulateOneStep(state: State): State = State((0 to 3).toArray.map(i => {
    val otherIndices = (0 to 3).filterNot(_ == i)
    val velocity = (0 to 2).toArray.map(d =>
      state.moons(i).velocity(d) + otherIndices.map(j =>
        signum(state.moons(j).position(d) - state.moons(i).position(d))).sum)
    val position = (0 to 2).toArray.map(d => state.moons(i).position(d) + velocity(d))
    Moon(position, velocity)
  }))

  private def printDebug(state: State, timeStep: Long): Unit = {
    println(s"After $timeStep steps:")
    println(state)
    println()
  }

  @scala.annotation.tailrec
  private def simulateLoop(state: State, step: Long, steps: Long, debug: Boolean = false): State = {
    val newState = simulateOneStep(state)
    if (debug && step % (steps / 10) == 0) printDebug(newState, step)
    if (step < steps) simulateLoop(newState, step+1, steps, debug)
    else newState
  }

  private val initialState = initState(inputs(0))

  def one: Int = {
    val debug = false
    if (debug) printDebug(initialState, 0)
    val finalState = simulateLoop(initialState, 1, 1000, debug)
    energy(finalState)
  }

  @scala.annotation.tailrec
  private def simulateUntil(prevStates: mutable.LinkedHashSet[State]): Long = {
    val newState = simulateOneStep(prevStates.last)
    if (prevStates(newState)) prevStates.dropWhile(s => s != newState).size
    else simulateUntil(prevStates.addOne(newState))
  }

  private val pv0s = (0 to 2).toArray.map(i => {
    val p = initialState.moons.map(_.position(i))
    val v = initialState.moons.map(_.velocity(i))
    (p, v)
  })

  private def simulateOneDimension(pv: (Array[Int], Array[Int])): (Array[Int], Array[Int]) = {
    val v = (0 to 3).toArray.map(i => {
      val others = (0 to 3).filter(_ != i)
      pv._2(i) + others.map(j => signum(pv._1(j) - pv._1(i))).sum
    })
    val p = (0 to 3).toArray.map(i => pv._1(i) + v(i))
    (p, v)
  }

  @scala.annotation.tailrec
  private def findRepeated(pv0: (Array[Int], Array[Int]), pv: (Array[Int], Array[Int]), steps: Long): Long = {
    val newPv = simulateOneDimension(pv)
    if (newPv._1.sameElements(pv0._1) && newPv._2.sameElements(pv0._2)) steps
    else findRepeated(pv0, newPv, steps+1)
  }

  def two: Long = {
//    val prevStates = mutable.LinkedHashSet(initialState)
//    simulateUntil(prevStates)
    val noSteps = pv0s.map(pv0 => findRepeated(pv0, pv0, 1))
    lcm(noSteps)
  }
}