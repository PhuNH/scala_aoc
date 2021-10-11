package y2019.w5

import common.Day
import common.Utils._
import y2019.Intcode

import scala.collection.mutable

case class IdleStatus(var isIdle: Boolean)

class Day23 extends Day(inputPath(2019, 23)) {
  private val codes: Array[Long] = Intcode.readCodes(inputs.head)
  private val computers = (0 until 50).toArray.map(i => {
    val program = Intcode(codes)
    program.setInput(i)
    val packetQueue = mutable.Queue.empty[(Long, Long)]
    val outputs = mutable.ArrayBuffer(0L)
    (program, outputs, packetQueue, IdleStatus(false))
  })

  private var natValue = (0L, 0L)
  private var prevSentToZero: Option[(Long, Long)] = None
  private var isNetworkIdle = false
  def loopThroughComputers(): Unit = {
    computers.zipWithIndex.foreach { case ((program, outputs, queue, IdleStatus(_)), index) =>
      program.run(outputs)
      computers(index)._4.isIdle = outputs(0) <= 0
      if (outputs(0) > 0) {
        println(s"output length: ${outputs(0)}")
        for (i <- Range(1, outputs(0).toInt, 3)) {
          if (outputs(i) == 255) {
            println(s"y value sent to address 255: ${outputs(i+2)}")
            natValue = (outputs(i+1), outputs(i+2))
          }
          if (outputs(i) < 50) {
            computers(outputs(i).toInt)._3.enqueue((outputs(i+1), outputs(i+2)))
          }
        }
      }

      outputs.clearAndShrink()
      outputs.append(0)
      computers(index)._4.isIdle &= queue.isEmpty
      if (queue.isEmpty) program.setInput(-1)
      else {
        val (x, y) = queue.dequeue()
        program.setInput(x)
        program.run(outputs)
        program.setInput(y)
      }
    }
  }

  def run(): Unit = {
    while (true) {
      loopThroughComputers()
      isNetworkIdle = computers.forall(_._4.isIdle)
      if (isNetworkIdle) {
        if (prevSentToZero.nonEmpty && prevSentToZero.get == natValue) {
          println(s"y value sent to address 0 twice: ${natValue._2}")
          return
        }
        prevSentToZero = Some(natValue)
        computers(0)._3.enqueue(natValue)
      }
    }
  }
  run()

  def one: Unit = {}

  def two: Unit = {}
}