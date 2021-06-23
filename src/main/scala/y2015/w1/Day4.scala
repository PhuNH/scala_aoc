package y2015.w1

import common.Day
import common.Utils._

import java.security.MessageDigest

class Day4 extends Day(inputPath(2015, 4)) {
//  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val data: String = "bgvyzdsv"

  private def byteArrayToHexString(ba: Array[Byte]): String = ba.map("%02X" format _).mkString

  private def md5(key: String): String =
    byteArrayToHexString(MessageDigest.getInstance("md5").digest(key.getBytes))

  private def checkHashX(hash: String)(x: Int): Boolean = hash.substring(0, x).forall(_ == '0')
  private def checkHash5(hash: String): Boolean = checkHashX(hash)(5)
  private def checkHash6(hash: String): Boolean = checkHashX(hash)(6)

  private def findSalt(s: String)(checkHash: String => Boolean): Int = {
    var i = 1
    while (true) {
      val key = s + i
      val hash = md5(key)
      if (checkHash(hash))
        return i
      i += 1
    }
    0
  }

  def one: Int = findSalt(data)(checkHash5)

  def two: Int = findSalt(data)(checkHash6)
}