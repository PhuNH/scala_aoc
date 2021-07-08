package y2015.w3

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day19 extends Day(inputPath(2015, 19)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val replacements = data.slice(0, data.length-2).map(l => {
    val parts = l.split(' ')
    (parts(0), parts(2))
  }).groupMap(_._1)(_._2)
  private val medicine = data(data.length-1)
  private val a2zC = 'A' to 'Z'

  private def parseFormula(formula: String): IndexedSeq[(String, Int)] = formula.zipWithIndex.tails.map(cis => {
    if (cis.nonEmpty && a2zC.contains(cis(0)._1)) {
      val nextCap = cis.indexWhere(ci => a2zC.contains(ci._1), 1)
      ((if (nextCap == -1) cis else cis.slice(0, nextCap)).map(_._1).mkString, cis(0)._2)
    } else ("", -1)
  }).toIndexedSeq.filter(_._2 != -1)

  private def replace(formula: String): Set[String] = if (formula == "e") {
    replacements("e").toSet
  } else {
    val substanceSeq = parseFormula(formula)
    substanceSeq.flatMap(si => {
      if (replacements.contains(si._1)) replacements(si._1).map(r => formula.patch(si._2, r, si._1.length))
      else Array.empty[String]
    }).toSet
  }

  def one: Int = replace(medicine).size

//  @tailrec
//  private def replaceRec(itms: Set[String], step: Int): Int =
//    if (itms.contains(medicine)) step
//    else {
//      val nextItms = itms.flatMap(replace)
//      println(s"${nextItms.size}; ${nextItms.map(_.length).max}")
//      replaceRec(nextItms, step+1)
//    }

  private val replacementValues = replacements.values.flatten.toSet
  private val maxValLen = replacementValues.map(_.length).max

  private def findTailSlice(tail: IndexedSeq[(String, Int)]): IndexedSeq[(String, Int)] = {
    val first = tail(0)
    val lastIndex = tail.indexWhere(t => t._2 + t._1.length - first._2 > maxValLen, 1)
    if (lastIndex != -1) tail.slice(0, lastIndex)
    else tail
  }

  private def unreplace(formula: String, step: Int): Int = if (formula == "e") {
    step
  } else {
    val substanceSeq = parseFormula(formula)
    val results = substanceSeq.tails.toIndexedSeq.map(tail =>
      if (tail.isEmpty) -1
      else {
        val tailSlice = findTailSlice(tail)
        val lengths = tailSlice.inits.toIndexedSeq.init.map(tailInit => {
          val tailInitVal = tailInit.map(_._1).mkString
          if (replacementValues.contains(tailInitVal)) {
            val unFormula =
              formula.patch(tailInit(0)._2, replacements.find(_._2.contains(tailInitVal)).get._1, tailInitVal.length)
            unreplace(unFormula, step+1)
          } else -1
        }).filter(_ != -1).toSet
        if (lengths.isEmpty) -1
        else lengths.min
      }
    ).filter(_ != -1).toSet
    if (results.isEmpty) -1
    else results.min
  }

  def two: Int = unreplace(medicine, 0)
  // it takes forever to run this brute-force solution.
  // from https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
  // token_count - Rn_count - Ar_count - 2 * Y_count - 1
}