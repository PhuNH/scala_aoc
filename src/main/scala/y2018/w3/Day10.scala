package y2018.w3

import common.Day
import common.Utils._

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.io.Source

class Day10 extends Day(inputPath(2018, 10)) {
  case class TwoD(x: Int, y: Int) {
    def +(that: TwoD): TwoD = TwoD(x+that.x, y+that.y)

    def *(n: Int): TwoD = TwoD(x*n, y*n)
  }

  case class Point(pos: TwoD, vel: TwoD) {
    def this(xp: Int, yp: Int, xv: Int, yv: Int) = this(TwoD(xp, yp), TwoD(xv, yv))
  }

  private val points: Array[Point] = using(Source.fromResource(inputs(0)))(
    _.getLines().toArray.map(l => {
      val splitted = l.split(Array('<', ',', '>'))
      new Point(splitted(1).trim.toInt, splitted(2).trim.toInt, splitted(4).trim.toInt, splitted(5).trim.toInt)
    }))

  private def move(pts: Array[Point], time: Int = 1): Array[Point] =
    pts.map(pt => Point(pt.pos+(if (time == 1) pt.vel else pt.vel * time), pt.vel))

  private def draw(pts: Array[Point], index: Int): Unit = {
    val ps = pts.map(pt => pt.pos)
    val pxs = ps.map(_.x)
    val pys = ps.map(_.y)
    println(s"${pxs.min} ${pxs.max} ${pys.min} ${pys.max}")

    val w = pxs.max-pxs.min+1
    val h = pys.max-pys.min+1
    val img = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY)
    for (x <- 0 until w)
      for (y <- 0 until h) {
        val color = if (ps.contains(TwoD(x+pxs.min, y+pys.min))) 0x000000 else 0xffffff
        img.setRGB(x, y, color)
      }

    ImageIO.write(img, "jpg", new File(s"$index.jpg"))
    println(s"$index")
  }

  def one: Unit = {
    var data = move(points, 10100)
    draw(data, 0)
    for (i <- 1 to 5) {
      data = move(data)
      draw(data, i)
    }
  }

  def two: Int = 10101
}