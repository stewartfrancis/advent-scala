package a2021

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object A23 extends App {
  // 1  2  3  4  5  6  7  8  9 10 11
  //      23    25    27    29
  //      43    45    47    49
  //      63    65    67    69
  //      83    85    87    89
  val pos0 = Range.inclusive(3, 9, 2)
  val corridor = Range.inclusive(1, 11).filter(!pos0.contains(_))
  val pos1 = Range.inclusive(23, 29, 2)
  val pos2 = Range.inclusive(43, 49, 2)
  val pos3 = Range.inclusive(63, 69, 2)
  val pos4 = Range.inclusive(83, 89, 2)
  val validIndices = (corridor ++ pos0 ++ pos1 ++ pos2 ++ pos3 ++ pos4).toList
  val neighbours = validIndices.map(i => (i, List(i - 1, i + 1, i - 20, i + 20).intersect(validIndices))).toMap
  val moveCost = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val finished = HashMap(
    23 -> 'A', 25 -> 'B', 27 -> 'C', 29 -> 'D',
    43 -> 'A', 45 -> 'B', 47 -> 'C', 49 -> 'D',
    63 -> 'A', 65 -> 'B', 67 -> 'C', 69 -> 'D',
    83 -> 'A', 85 -> 'B', 87 -> 'C', 89 -> 'D'
  )

  val state1 = State(
    Map(
      23 -> 'B', 25 -> 'B', 27 -> 'C', 29 -> 'D',
      43 -> 'D', 45 -> 'C', 47 -> 'A', 49 -> 'A',
      63 -> 'A', 65 -> 'B', 67 -> 'C', 69 -> 'D',
      83 -> 'A', 85 -> 'B', 87 -> 'C', 89 -> 'D'
    ), 0)
  println(solve(List(state1), Int.MaxValue))

  val state2 = State(
    Map(
      23 -> 'B', 25 -> 'B', 27 -> 'C', 29 -> 'D',
      43 -> 'D', 45 -> 'C', 47 -> 'B', 49 -> 'A',
      63 -> 'D', 65 -> 'B', 67 -> 'A', 69 -> 'C',
      83 -> 'D', 85 -> 'C', 87 -> 'A', 89 -> 'A',
    ), 0)
  println(solve(List(state2), Int.MaxValue))

  case class State(m: Map[Int, Char], e: Int)

  @tailrec
  def solve(states: Iterable[State], minCost: Int): Int = {
    if (states.isEmpty) {
      minCost
    } else {
      val (done: List[State], notDone: List[State]) = states
        .filter(_.e < minCost)
        .flatMap(state => {
          state.m
            .filter { case (cell, char) => !isHome(cell, char, state.m) }
            .flatMap { case (cell, char) => makeNextMoves(cell, char, state) }
        })
        .partition(_.m == finished)
      solve(notDone.distinct, Math.min(minCost, done.map(_.e).minOption.getOrElse(Int.MaxValue)))
    }
  }

  def isHome(cell: Int, char: Char, state: Map[Int, Char]): Boolean = {
    cell match {
      case x if x > 20 =>
        val targetDigit = x match {
          case y if y % 10 == 3 => 'A'
          case y if y % 10 == 5 => 'B'
          case y if y % 10 == 7 => 'C'
          case y if y % 10 == 9 => 'D'
        }
        char == targetDigit && Iterator
          .iterate(x + 20)(_ + 20)
          .takeWhile(_ < 90)
          .forall { n => state.get(n).contains(targetDigit) }
      case _ => false
    }
  }

  def makeNextMoves(cell: Int, char: Char, state: State): List[State] = {
    val init = neighbours(cell).filter(!state.m.contains(_)).map(_ :: cell :: Nil)
    Iterator
      .unfold(init)(x => Option(x)
        .filter(_.nonEmpty)
        .map(paths => (
          paths,
          paths.flatMap { path =>
            path match {
              case current :: prev :: _ => neighbours(current)
                .filter(_ != prev)
                .filter(!state.m.contains(_))
                .map(_ :: path)
            }
          }
        ))
      )
      .flatten
      .filter { case destination :: _ =>
        isHome(destination, char, state.m) || //Moving to the correct home square
          (!corridor.contains(cell) && corridor.contains(destination)) //Moving into the corridor
      }
      .map(move => State(state.m.removed(cell).updated(move.head, char), state.e + (moveCost(char) * (move.size - 1))))
      .toList
  }
}
