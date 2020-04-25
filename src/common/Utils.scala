package common

import scala.io.Source

object Utils {
  def inputPath(year: Int, day: Int) = s"/mnt/d/Projects/Scala/AoC/input/$year/$day"

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}
