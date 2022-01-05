package a2021

import scala.annotation.tailrec

object A4 extends App {
  type Board = Array[Array[(Int, Boolean)]]

  def lines = util.loadInputLines(getClass).toList

  val (numbers, boards: List[Board]) = lines match { //parse input
    case ::(head, next) => (
      head.split(",").map(Integer.parseInt).toList,
      next
        .grouped(6)
        .map(_
          .drop(1)
          .map(_.trim.split("\\s+").map(s => (Integer.parseInt(s), false)))
          .toArray
        )
        .toList
    )
  }

  val r = Range(0, 5, 1)

  def checker(cells: Seq[(Int, Int)])(board: Board): Boolean = {
    cells.iterator.forall(c => board(c._1)(c._2)._2)
  }

  def coords = r.flatMap(x => r.map { y =>
    val row = r.filter(_ != y).map(yy => (x, yy))
    val col = r.filter(_ != x).map(xx => (xx, y))
    (x, y, checker(row) _, checker(col) _)
  })

  def solveBoard(board: Board, number: Int) = {
    def c = coords.iterator.find(c => board(c._1)(c._2)._1 == number)

    c.exists { c =>
      val matched = board(c._1)(c._2) //coord found, set true and check win
      board(c._1)(c._2) = matched.copy(_2 = true) //mark matched (side-effect!)
      c._3(board) || c._4(board)
    }
  }

  def score(lastNumber: Int, board: Board) = board.flatten.filter(!_._2).map(_._1).sum * lastNumber

  @tailrec
  def findSolution(numbers: List[Int], boards: List[Board]): (List[Int], List[Int], List[Board]) =
    numbers match {
      case ::(next, rest) =>
        println(next)
        boards.partition(solveBoard(_, next)) match {
          case (solved, unsolved) =>
            if (solved.nonEmpty) {
              (solved.map(score(next, _)), rest, unsolved)
            } else {
              findSolution(rest, unsolved)
            }
        }
    }

  val i = Iterator.iterate(findSolution(numbers, boards))(t => findSolution(t._2, t._3))

  println(i.next()._1)
  println(i.dropWhile(_._3.nonEmpty).next()._1)
}
