package y2020.w3

import common.Day
import common.Utils._

import scala.io.Source

class Day19 extends Day(inputPath(2020, 19), testPath(2020, 19, 1)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val empty = data.indexWhere(_ == "")
  private val (ruleStrings, messages) = { val pair = data.splitAt(empty); (pair._1, pair._2.tail) }
  private var ruleStringsMap = ruleStrings.map(s => { val pair = s.split(':'); (pair(0), pair(1).trim) }).toMap
  private var rules = Map.empty[String, Array[String]]

  private def processPlainRule(content: String): Array[String] = {
    val subRuleNames = content.split(' ')
    val firstResult = processRule(subRuleNames(0))
    if (subRuleNames.length == 1) firstResult
    else subRuleNames.tail.map(processRule).foldLeft(firstResult)(
      (b, a) => for { start <- b; end <- a } yield start + end)
  }

  private def processRule(name: String): Array[String] = if (rules.contains(name)) rules(name)
  else {
    val content = ruleStringsMap(name)
    val result =
      if (content(0) == '"') Array(content.substring(1, content.length-1))
      else if (content.contains('|')) {
        val options = content.split('|').map(_.trim)
        options.map(processPlainRule).reduce(_ ++ _)
      } else processPlainRule(content)
    rules = rules.updated(name, result)
    result
  }

  private var regexRules = Map.empty[String, String]

  private def makePlainRegexRule(content: String): String = content.split(' ').map(makeRegexRule).reduce((a1, a2) =>
    (if (a1.length > 1) '(' + a1 + ')' else a1) + (if (a2.length > 1) '(' + a2 + ')' else a2))

  private def makeRegexRule(name: String): String =
    if (regexRules.contains(name)) regexRules(name)
    else {
      val content = ruleStringsMap(name)
      val result =
        if (content(0) == '"') content.substring(1, content.length-1)
        else if (content.contains('|')) {
          val options = content.split('|').map(_.trim)
          options.map(makePlainRegexRule).mkString("|")
        } else makePlainRegexRule(content)
      regexRules = regexRules.updated(name, result)
      result
    }

  def one: Int = {
//    messages.tail.count(processRule("0").contains)
    val regex0 = makeRegexRule("0").r
    messages.count(regex0.matches)
  }

  def two: Int = {
    ruleStringsMap = ruleStringsMap.updated("8", "42 | 42 8").updated("11", "42 31 | 42 11 31")
//    messages.tail.count(processRule("0").contains)
    val regexRule42 = regexRules("42")
    val regexRule31 = regexRules("31")
    val newRegexRule11 = (for { i <- 1 to 5; j <- 1 to i }
      yield Array.fill(j)(s"($regexRule42)").mkString("") + Array.fill(j)(s"($regexRule31)").mkString("")).mkString("|")
    regexRules = regexRules.updated("8", s"($regexRule42)+").updated("11", newRegexRule11)
    regexRules = regexRules.updated("0", s"(${regexRules("8")})(${regexRules("11")})")
    val regex0 = makeRegexRule("0").r
    messages.count(regex0.matches)
  }
}