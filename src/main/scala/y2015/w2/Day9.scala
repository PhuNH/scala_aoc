package y2015.w2

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day9 extends Day(inputPath(2015, 9)) {
  type Distances = Map[String, Int]
  type Locations = Map[String, Distances]

  private def processDataLine(line: String, locations: Locations): Locations = {
    val parts = line.split(" = ")
    val words = parts(0).split(' ')
    val distance = parts(1).toInt
    locations.updated(words(0), locations.getOrElse(words(0), Map.empty[String, Int]).updated(words(2), distance))
      .updated(words(2), locations.getOrElse(words(2), Map.empty[String, Int]).updated(words(0), distance))
  }

  private val data: Array[String] = using(Source.fromResource(inputs(0)))(_.getLines().toArray)
  private val locations: Locations =
    data.foldRight(Map.empty[String, Distances])(processDataLine)

  private def doPrimLike(locations: Locations): (List[String], Int) = {
    @tailrec
    def doPrimLikeRec(visited: List[String], accDist: Int): (List[String], Int) =
      locations(visited.head).toArray.sortBy(_._2).reverse.find(d => !visited.contains(d._1)) match {
        case None => (visited, accDist)
        case Some((l, d)) => doPrimLikeRec(l :: visited, accDist + d)
      }

    val minFromLocations = locations.keys.map(l => doPrimLikeRec(List(l), 0))
    minFromLocations.maxBy(_._2)
  }

  private def doBruteForce(locations: Locations): (List[String], Int) = {
    def doBruteForceRec(locations: Locations, visited: List[String], accDist: Int): Array[(List[String], Int)] = {
      if (locations.size == 1) Array((visited, accDist))
      else {
        val starts = locations.removed(visited.head)
        starts.keys.toArray.flatMap(l => doBruteForceRec(starts, l :: visited, accDist + locations(visited.head)(l)))
      }
    }

    val minFromLocations = locations.keys.map(l => doBruteForceRec(locations, List(l), 0).maxBy(_._2))
    minFromLocations.maxBy(_._2)
  }

  def one: Int = {
    println(doPrimLike(locations))
    println(doBruteForce(locations))
    1
  }

  def two: Unit = {}

  /* The result of doPrimLike for min happens to be correct, but the one for max is incorrect
  Returned from doPrimLike:
  0 = {Tuple2@1327} "(List(Tambi, AlphaCentauri, Straylight, Snowdin, Arbre, Tristram, Norrath, Faerun),730)"
  1 = {Tuple2@1328} "(List(Faerun, Tambi, Straylight, AlphaCentauri, Tristram, Norrath, Arbre, Snowdin),756)"
  2 = {Tuple2@1329} "(List(Faerun, Tambi, AlphaCentauri, Tristram, Norrath, Arbre, Snowdin, Straylight),712)"
  3 = {Tuple2@1330} "(List(Faerun, Tambi, AlphaCentauri, Straylight, Snowdin, Arbre, Norrath, Tristram),685)"
  4 = {Tuple2@1331} "(List(Faerun, Tambi, Straylight, Snowdin, Arbre, Norrath, Tristram, AlphaCentauri),764)"
  5 = {Tuple2@1332} "(List(Faerun, Tambi, AlphaCentauri, Straylight, Snowdin, Arbre, Tristram, Norrath),672)"
  6 = {Tuple2@1333} "(List(Faerun, AlphaCentauri, Straylight, Snowdin, Arbre, Tristram, Norrath, Tambi),678)"
  7 = {Tuple2@1334} "(List(Tambi, Faerun, Snowdin, Straylight, AlphaCentauri, Tristram, Norrath, Arbre),716)"

  Returned from doBruteForce:
  0 = {Tuple2@1369} "(List(Straylight, Snowdin, Arbre, AlphaCentauri, Tristram, Norrath, Faerun, Tambi),804)"
  1 = {Tuple2@1370} "(List(Faerun, Tambi, Straylight, Snowdin, Arbre, AlphaCentauri, Tristram, Norrath),745)"
  2 = {Tuple2@1371} "(List(Tambi, Faerun, Norrath, Tristram, Arbre, Snowdin, Straylight, AlphaCentauri),783)"
  3 = {Tuple2@1372} "(List(Straylight, Tambi, Faerun, Norrath, Tristram, AlphaCentauri, Arbre, Snowdin),775)"
  4 = {Tuple2@1373} "(List(Tambi, Faerun, Norrath, Arbre, Snowdin, Straylight, AlphaCentauri, Tristram),772)"
  5 = {Tuple2@1374} "(List(Tambi, Faerun, Norrath, Tristram, AlphaCentauri, Straylight, Snowdin, Arbre),779)"
  6 = {Tuple2@1375} "(List(Tambi, Faerun, Norrath, Tristram, AlphaCentauri, Arbre, Snowdin, Straylight),804)"
  7 = {Tuple2@1376} "(List(Tambi, Straylight, Snowdin, Arbre, AlphaCentauri, Tristram, Norrath, Faerun),803)"

  The correct answer is the route with length 804 starting from Tambi. Going doPrimLike from Tambi leads to Norrath
  since the furthest away from Tambi is Norrath (82), but actually the route from Tambi to Faerun (71) should be chosen.
   */
}