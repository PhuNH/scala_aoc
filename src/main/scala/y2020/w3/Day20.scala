package y2020.w3

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day20 extends Day(inputPath(2020, 20), testPath(2020, 20, 1)) {
  type Grid = Array[Array[Char]]
  type Borders = Array[Array[Char]]
  type Matches = Array[Int]
  type Data = (Grid, Borders)

  private def flipArray(a: Array[Char]): Array[Char] = a.indices.toArray.map(i => a(a.length-1-i))

  @tailrec
  private def transformBorders(borders: Borders, method: Int, extraParam: Int = 0): Borders = method match {
    case 1 => // rotate clockwise
      val rotation = Array(borders(3), flipArray(borders(0)), borders(1), flipArray(borders(2)))
      if (extraParam <= 1) rotation
      else transformBorders(rotation, method, extraParam - 1)
    case 2 => Array(borders(2), flipArray(borders(1)), borders(0), flipArray(borders(3))) // flip horizontally
      // H() = V(R180()) = D(R270())
      // 0: 0, 1, 2, 3      H: 2, f1, 0, f3
      //    3, f0, 1, f2       1, 0, 3, 2
      //    f2, f3, f0, f1     f0, 3, f2, 1
      //    f1, 2, f3, 0       f3, f2, f1, f0
    case _ => borders
  }

  @tailrec
  private def transformGrid(grid: Grid, method: Int, extraParam: Int = 0): Grid = method match {
    case 1 =>
      val rotation = grid(0).indices.toArray.map(i => grid.indices.toArray.map(j => grid(grid.length-1-j)(i)))
      if (extraParam <= 1) rotation
      else transformGrid(rotation, method, extraParam-1)
    case 2 => grid.indices.toArray.map(i => flipArray(grid(i)))
    case _ => grid
  }

  case class Tile(number: Int, grid: Grid, borders: Borders)

  private val tiles: Map[Int, Tile] =
    using(Source.fromResource(inputs(0)))(_.getLines().mkString("\n").split("\n\n").map(tile => {
      val tileLines = tile.split("\n")
      val tileNumber = tileLines(0).split(Array(' ', ':'))(1).toInt
      val tileData = tileLines.tail.map(_.toCharArray)
      val borders = Array(tileData.indices.toArray.map(tileData(_)(0)), tileData(0),
        tileData.indices.toArray.map(tileData(_)(tileData(0).length-1)), tileData(tileData.length-1))
      (tileNumber, Tile(tileNumber, tileData, borders))
    })).toMap

  private val matchesOfTiles: Map[Int, Matches] = tiles.map(tile => {
    val otherTileNumbers = tiles.keys.filterNot(_ == tile._1)
    val matches = tile._2.borders.indices.toArray.map(i =>
      otherTileNumbers.find(num => tiles(num).borders.exists(_ sameElements tile._2.borders(i)) ||
        tiles(num).borders.exists(_ sameElements flipArray(tile._2.borders(i)))) match {
        case None => 0
        case Some(n) => n
      })
    (tile._1, matches)
  })

  private val matchesOfCorners = matchesOfTiles.filter(_._2.count(_ == 0) == 2)

  def one: Long = matchesOfCorners.map(_._1.toLong).product

  private def findTrans(matches: Matches, requirements: Array[Int]): Array[Int] = {
    def checkPair(p: Array[Int]): Array[Int] = p match {
      case Array(0,1) => Array(0)
      case Array(1,2) => Array(1,3)
      case Array(2,3) => Array(1,2)
      case Array(3,0) => Array(1,1)
      case Array(0,3) => Array(2,1,2)
      case Array(3,2) => Array(2,1,1)
      case Array(2,1) => Array(2)
      case Array(1,0) => Array(2,1,3)
      case _ => Array()
    }

    if (!requirements.forall(matches.contains)) Array()
    else if (requirements sameElements Array(0,0)) {
      val indices = Array(matches.indexOf(0), matches.lastIndexOf(0))
      if (indices(0) == indices(1)) Array()
      else checkPair(indices)
    } else if (requirements.contains(0) && matches.count(_ == 0) == 2) {
      val res = checkPair(requirements.map(matches.indexOf(_)))
      if (res.nonEmpty) res else checkPair(requirements.map(matches.lastIndexOf(_)))
    } else checkPair(requirements.map(matches.indexOf(_)))
  }

  @tailrec
  private def bulkTransform(trans: Array[Int], data: Data): Data = {
    if (trans.isEmpty) data
    else {
      val newData = trans(0) match {
        case 1 => (transformGrid(data._1, 1, trans(1)), transformBorders(data._2, 1, trans(1)))
        case 2 => (transformGrid(data._1, 2), transformBorders(data._2, 2))
        case _ => data
      }
      val newTrans = trans(0) match {
        case 1 => trans.slice(2, trans.length)
        case _ => trans.tail
      }
      bulkTransform(newTrans, newData)
    }
  }

  private def trimGrid(grid: Grid): Grid = grid.slice(1, grid.length-1).map(_.slice(1, grid(0).length-1))

  private def buildLand: Grid = {
    val landSize = math.sqrt(tiles.size).toInt
    val tileSize = tiles.head._2.grid.length - 2
    val landMap = Array.fill(landSize)(new Array[Tile](landSize))

    def getNewTile(num: Int, trans: Array[Int]): Tile = {
      val tile = tiles(num)
      val data = bulkTransform(trans, (tile.grid, tile.borders))
      Tile(num, trimGrid(data._1), data._2)
    }

    def makeLandTile(requirements: Array[Int]): Tile = {
      val matchTileAndTrans = matchesOfTiles.zip(
        matchesOfTiles.map(matchesOfTile => findTrans(matchesOfTile._2, requirements)))
      val filtered = matchTileAndTrans.filter(e => e._2.nonEmpty &&
        !landMap.flatten.filterNot(_ == null).map(_.number).contains(e._1._1))
      getNewTile(filtered.head._1._1, filtered.head._2)
    }

    landMap(0)(0) = matchesOfCorners.find(cornerMatches => {
      val matches = cornerMatches._2
      matches(0) == 0 && matches(1) == 0
    }) match {
      case Some(c) =>
        val tile = tiles(c._1)
        Tile(tile.number, trimGrid(tile.grid), tile.borders)
      case None =>
        val someMatch = matchesOfCorners.head
        val trans = findTrans(someMatch._2, Array(0, 0))
        getNewTile(someMatch._1, trans)
    }
    for (j <- 1 until landSize) {
      val requirements = Array(landMap(0)(j-1).number, 0)
      landMap(0)(j) = makeLandTile(requirements)
    }
    for (i <- 1 until landSize) {
      val requirements = Array(0, landMap(i-1)(0).number)
      landMap(i)(0) = makeLandTile(requirements)
    }
    for (i <- 1 until landSize) for (j <- 1 until landSize) {
      val requirements = Array(landMap(i)(j-1).number, landMap(i-1)(j).number)
      landMap(i)(j) = makeLandTile(requirements)
    }
    val land = (0 until tileSize*landSize).toArray.map(i => {
      val (iTile, iLand) = (i % tileSize, i / tileSize)
      landMap(iLand).map(_.grid(iTile)).reduce(_ ++ _)
    })
    land
  }

  def hasMonster(grid: Grid, i: Int, j: Int): Boolean = {
    def at(di: Int, dj: Int) = grid(i+di)(j+dj)

    val pos = Array((0,18), (1,0), (1,5), (1,6), (1,11), (1,12), (1,17), (1,18), (1,19),
      (2,1), (2,4), (2,7), (2,10), (2,13), (2,16))
    val res = pos.map(p => at(p._1,p._2)).forall(_ == '#')
    if (res) pos.foreach(p => grid(p._1+i)(p._2+j) = 'O')
    res
  }

  def findMonster(grid: Grid): Int = {
    var i = 0
    while (i <= grid.length - 3) {
      var j = 0
      while (j <= grid(0).length - 20) if (hasMonster(grid, i, j)) j += 20 else j += 1
      i += 1
    }
    val d = grid.flatten
    if (d.contains('O')) d.count(_ == '#')
    else 0
  }

  def two: Int = {
    val land = buildLand
    val rotation1 = transformGrid(land, 1)
    val rotation2 = transformGrid(rotation1, 1)
    val rotation3 = transformGrid(rotation2, 1)
    val rotations = Array(land, rotation1, rotation2, rotation3)
    val trans = rotations ++ rotations.map(r => transformGrid(r, 2))
    for (t <- trans) {
      val res = findMonster(t)
      if (res > 0) return res
    }
    0
  }
}