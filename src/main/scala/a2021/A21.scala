package a2021

import scala.annotation.tailrec

object A21 extends App {
  val lines = util.loadInputLines(getClass)

  case class Game(nextTurn: Player, lastTurn: Player) {
    def advance(positions: Int): Game = {
      Game(lastTurn, nextTurn.advance(positions))
    }
  }

  case class Player(id: Int, index: Int, score: Int) {
    def advance(positions: Int): Player = {
      val newPosition = (index + positions) % 10
      Player(id, newPosition, score + newPosition + 1)
    }
  }

  val r = ".*: (.*)".r
  val startState = lines
    .map { case r(startIndex) => startIndex.toInt }
    .toList match {
    case List(a, b) => Game(Player(1, a - 1, 0), Player(2, b - 1, 0))
  } //make 0-indexed

  val die = Iterator.from(1)
  val endState = Iterator
    .iterate(startState) { g => g.advance(die.take(3).sum) }
    .dropWhile {
      _.lastTurn.score < 1000
    }
    .next()

  //1
  println(endState.nextTurn.score * (die.next() - 1))

  val diracDiceOccurrences = List(1, 1, 1, 2, 2, 2, 3, 3, 3)
    .combinations(3)
    .flatMap(_.permutations.map(_.sum))
    .toList
    .groupBy(identity)
    .map(i => (i._1, i._2.size))
    .toList

  @tailrec
  def playDirac(games: List[(Game, Long)], wins1: Long, wins2: Long): (Long, Long) = games match {
    case ::((game, instances), rest) =>
      if (game.lastTurn.score >= 21) {
        if (game.lastTurn.id == 1) {
          playDirac(rest, wins1 + instances, wins2)
        } else {
          playDirac(rest, wins1, wins2 + instances)
        }
      } else {
        playDirac(diracDiceOccurrences.map(d => (game.advance(d._1), instances * d._2)) ++ rest, wins1, wins2)
      }
    case Nil => (wins1, wins2)
  }

  //2
  val results = playDirac(List((startState, 1)), 0, 0)
  println(Math.max(results._1, results._2))
}
