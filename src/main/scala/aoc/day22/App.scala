package aoc.day22

import aoc.shared.Reader

import scala.annotation.tailrec

case class Game(deck1: List[Int],
                deck2: List[Int],
                previousRounds: List[(List[Int], List[Int])] = List.empty,
                winner: Option[Int] = None) {

  def decksEmpty: Boolean =
    deck1.isEmpty || deck2.isEmpty

  def over: Boolean =
    winner.isDefined || decksEmpty

  def score: Int = {
    val winningDeck = List(deck1, deck2).filterNot(_.isEmpty).head

    winningDeck.reverse.zipWithIndex.map(t => t._1 * (t._2 + 1)).sum
  }

  def winningPlayer: Int =
    winner.getOrElse(
      List(deck1, deck2).zipWithIndex.filterNot(_._1.isEmpty).head._2 + 1
    )
}


object App {
  def main(args: Array[String]): Unit = {
    val groups = Reader.groups("22.txt")

    val decks = groups.map(_.tail.map(_.toInt))

    val game = Game(decks(0), decks(1))
    val part1 = play(game)

    println(part1.score)

    val part2 = play2(game)
    println(part2.score)
  }

  @tailrec
  def play(g: Game): Game = {
    if(g.over)
      g
    else {
      val card1 = g.deck1.head
      val card2 = g.deck2.head

      val newGame = if(card1 > card2)
        g.copy(deck1 = g.deck1.tail ++ List(card1, card2), deck2 = g.deck2.tail)
      else
        g.copy(deck1 = g.deck1.tail, deck2 = g.deck2.tail ++ List(card2, card1))

      play(newGame)
    }
  }

  def play2(g: Game): Game = {
    if (g.over) {
      g
    } else {
      val newGame = if (g.previousRounds.contains((g.deck1, g.deck2)))
        g.copy(winner = Some(1))
      else {
        val newG = g.copy(previousRounds = g.previousRounds :+ (g.deck1, g.deck2))

        val card1 = newG.deck1.head
        val card2 = newG.deck2.head

        if (card1 <= newG.deck1.tail.size && card2 <= newG.deck2.tail.size) {
          // this is a recursive game
          val recurGame = Game(
            deck1 = newG.deck1.tail.slice(0, card1),
            deck2 = newG.deck2.tail.slice(0, card2)
          )
          val p = play2(recurGame)
          p.winningPlayer match {
            case 1 =>
              newG.copy(deck1 = newG.deck1.tail ++ List(card1, card2), deck2 = newG.deck2.tail)
            case 2 =>
              newG.copy(deck1 = newG.deck1.tail, deck2 = newG.deck2.tail ++ List(card2, card1))
          }

        } else {
          if (card1 > card2)
            newG.copy(deck1 = newG.deck1.tail ++ List(card1, card2), deck2 = newG.deck2.tail)
          else
            newG.copy(deck1 = newG.deck1.tail, deck2 = newG.deck2.tail ++ List(card2, card1))
        }
      }
      play2(newGame)
    }
  }
}