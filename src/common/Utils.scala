package common

object Utils {
  def inputPath(year: Int, day: Int) = s"/mnt/d/Projects/Scala/AoC/input/$year/$day"

  def testPath(year: Int, day: Int, number: Int) = s"/mnt/d/Projects/Scala/AoC/input/$year/test/$day.$number"

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}
