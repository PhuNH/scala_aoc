package y2020.w1

import common.Day
import common.Utils._

import scala.io.Source

class Day4 extends Day(inputPath(2020, 4)) {
  private val fields = Array("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  private val passports = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
    .reduce(_ + " " + _).split(" {2}")
    .map(_.split(' ')
      .map(f => {
        val kv = f.split(':')
        (kv(0), kv(1))
      }).toMap[String, String])

  private val realPassports = passports.filter(
    _.keys.foldLeft(fields)((fs, k) => fs.filterNot(_ == k)).isEmpty)

  def one: Int = realPassports.length

  private def check(passport: Map[String, String]): Boolean = {
    "[0-9]{4}".r.matches(passport("byr")) && Range.inclusive(1920, 2002).contains(passport("byr").toInt) &&
      "[0-9]{4}".r.matches(passport("iyr")) && Range.inclusive(2010, 2020).contains(passport("iyr").toInt) &&
      "[0-9]{4}".r.matches(passport("eyr")) && Range.inclusive(2020, 2030).contains(passport("eyr").toInt) && {
      val hgtPat = "([0-9]{2,})(cm|in)".r
      passport("hgt") match {
        case hgtPat(number, unit) =>
          if (unit == "cm") Range.inclusive(150, 193).contains(number.toInt)
          else Range.inclusive(59, 76).contains(number.toInt)
        case _ => false
      }
    } &&
    "#[0-9a-z]{6}".r.matches(passport("hcl")) &&
    "amb blu brn gry grn hzl oth".split(' ').contains(passport("ecl")) &&
    "[0-9]{9}".r.matches(passport("pid"))
  }

  def two: Int = realPassports.count(check)
}
