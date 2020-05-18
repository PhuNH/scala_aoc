package common

object Utils {
  def inputPath(year: Int, day: Int) = s"/mnt/d/Projects/aoc/input/$year/$day"

  def testPath(year: Int, day: Int, number: Int) = s"/mnt/d/Projects/aoc/input/$year/test/$day.$number"

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a%b)

  def lcm(a: Long, b: Long): Long = (a*b).abs / gcd(a,b)

  def lcm(array: Array[Long]): Long = array.reduce(lcm)
}
