package y2020.w4

import common.Day
import common.Utils._

import scala.annotation.tailrec
import scala.io.Source

class Day22 extends Day(inputPath(2020, 22), testPath(2020, 22, 1)) {
  type Deck = Array[Int]

  private val decks: Array[Deck] = using(Source.fromResource(inputs(0)))(
    _.getLines().mkString("\n").split("\n\n").map(_.split("\n").tail.map(_.toInt)))

  private def score(deck: Deck): Int = deck.indices.map(i => deck(i) * (deck.length-i)).sum

  private def isRecursable(deckOne: Deck, deckTwo: Deck): Boolean =
    deckOne.tail.length >= deckOne(0) && deckTwo.tail.length >= deckTwo(0)

  @tailrec
  private def round(deckOne: Deck, deckTwo: Deck,
                    recursive: Boolean = false, prevRounds: Array[(Deck, Deck)] = Array()): (Deck, Int) = {
    if (recursive && prevRounds.exists(r => (r._1 sameElements deckOne) && (r._2 sameElements deckTwo))) (deckOne, 1)
    else if (deckOne.nonEmpty && deckTwo.nonEmpty) {
      val winner =
        if (recursive && isRecursable(deckOne, deckTwo))
          game(deckOne.slice(1, 1+deckOne(0)), deckTwo.slice(1, 1+deckTwo(0)), recursive)._2
        else if (deckOne(0) > deckTwo(0)) 1 else 2
      val (nextDeckOne, nextDeckTwo) =
        if (winner == 1) (deckOne.tail.appendedAll(Array(deckOne(0), deckTwo(0))), deckTwo.tail)
        else (deckOne.tail, deckTwo.tail.appendedAll(Array(deckTwo(0), deckOne(0))))
      if (recursive) round(nextDeckOne, nextDeckTwo, recursive, prevRounds :+ (deckOne, deckTwo))
      else round(nextDeckOne, nextDeckTwo)
    } else if (deckOne.nonEmpty) (deckOne, 1) else (deckTwo, 2)
  }

  private def game(deckOne: Deck, deckTwo: Deck, recursive: Boolean = false): (Deck, Int) =
    round(deckOne, deckTwo, recursive)

  def one: Int = score(game(decks(0), decks(1))._1)

  def two: Int = score(game(decks(0), decks(1), recursive = true)._1)
}