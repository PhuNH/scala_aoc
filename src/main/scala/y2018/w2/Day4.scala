package y2018.w2

import java.time.format.DateTimeFormatter
import java.time.LocalDate

import common.Day
import common.Utils._

import scala.io.Source

class Day4 extends Day(inputPath(2018, 4)) {
  private val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  private val idPattern = "#([0-9]+)".r
  private var events = Map.empty[LocalDate, (Int, Array[Int])]
  using(Source.fromResource(inputs(0)))(_.getLines().toList).foreach(eventString => {
    val fs = eventString.tail.split(']')
    val dateTime = fs(0).split(' ')
    val date = LocalDate.from(dateFormat.parse(dateTime(0)))
    val hourMinute = dateTime(1).split(':').map(_.toInt)
    fs(1).trim.split(' ')(1) match {
      case idPattern(id) =>
        val actualDate = if (hourMinute(0) == 23) date.plusDays(1) else date
        val actualMinute = if (hourMinute(0) == 23) 0 else hourMinute(1)
        val changes =
          if (!events.contains(actualDate)) Array(actualMinute)
          else events(actualDate)._2 :+ actualMinute
        events = events.updated(actualDate, (id.toInt, changes))
      case _ =>
        val content =
          if (!events.contains(date)) (-1, Array(hourMinute(1)))
          else (events(date)._1, events(date)._2 :+ hourMinute(1))
        events = events.updated(date, content)
    }
  })

  private def transform(changes: Array[Int]): Array[Int] = {
    val sorted = changes.sorted.tail
    Range(0, sorted.length, 2).toArray.flatMap(i => Range(sorted(i), sorted(i+1)))
  }

  private val sleepsGroupedByGuard = events.values.groupBy(_._1).view.mapValues(_.flatMap(c => transform(c._2)).toArray).toMap

  def one: Int = {
    val mostSleepingGuard = sleepsGroupedByGuard.maxBy(_._2.length)
    val mostSleptMinute = mostSleepingGuard._2.toSet.map((m: Int) => (m, mostSleepingGuard._2.count(_ == m))).maxBy(_._2)._1
    mostSleepingGuard._1 * mostSleptMinute
  }

  def two: Int = {
    val mostSleptMinuteOfGuards = sleepsGroupedByGuard.view.mapValues(sleeps =>
      if (sleeps.isEmpty) (-1, 0)
      else sleeps.toSet.map((m: Int) => (m, sleeps.count(_ == m))).maxBy(_._2)).toMap
    val guardWithMostSleptMinute = mostSleptMinuteOfGuards.maxBy(_._2._2)
    guardWithMostSleptMinute._1 * guardWithMostSleptMinute._2._1
  }
}
