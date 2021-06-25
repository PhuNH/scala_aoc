package y2015.w2

import common.Day
import common.Utils._

import scala.io.Source
import scala.util.matching.Regex

class Day8 extends Day(inputPath(2015, 8)) {
  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)

  private def hex2byte(hex: String) = Integer.parseInt(hex, 16).toByte

  private def decodeHexadecimals(str: String, encoding: String="UTF-8") =
    new String(str.split("""\\x""").tail.map(hex2byte), encoding)

  private def replaceHexadecimals(str: String, encoding: String="UTF-8") =
    """(\\x[\da-f]{2})""".r.replaceAllIn(str, m =>
      Regex.quoteReplacement(decodeHexadecimals(m.group(0), encoding)))

  private def unescape(s: String): Int = {
    val inMemory = replaceHexadecimals(s).tail.init
      .replaceAll("\\\\\\\\", Regex.quoteReplacement("\\"))
      .replaceAll("\\\\\\\"", Regex.quoteReplacement("\""))
    s.length - inMemory.length
  }

  def one: Int = data.map(unescape).sum

  private def escape(s: String): Int = {
    val code = s.replaceAll("\\\\", Regex.quoteReplacement("\\\\"))
      .replaceAll("\\\"", Regex.quoteReplacement("\\\""))
    2 + code.length - s.length
  }

  def two: Int = data.map(escape).sum
}