package a2021

import scala.annotation.tailrec

object A25 extends App {
  val lines = util.loadInputLines(getClass)

  val grid = lines.map(_.toCharArray).toArray
  val maxY = grid.length
  val maxX = grid.head.length
  val cukes = grid.indices.flatMap(y => grid.head.indices.map(x => (y, x)))
    .filter { case (y, x) => grid(y)(x) != '.' }
    .map { case (y, x) => ((y, x), grid(y)(x) == '>') }
    .toMap

  println(step(cukes, 1))

  @tailrec
  def step(cukes: Map[(Int, Int), Boolean], i: Int): Int = {
    val (eCukes, eMoved) = move(cukes, moveEast = true)
    val (sCukes, sMoved) = move(eCukes, moveEast = false)
    if (!eMoved && !sMoved) {
      i
    } else {
      step(sCukes, i + 1)
    }
  }

  def move(cukes: Map[(Int, Int), Boolean], moveEast: Boolean) = {
    cukes
      .filter { case (_, e) => e == moveEast }
      .map { case ((y, x), _) => ((y, x), if (moveEast) {
        (y, (x + 1) % maxX)
      } else {
        ((y + 1) % maxY, x)
      })}
      .filterNot { case (_, t) => cukes.contains(t) }
      .foldLeft((cukes, false)) { case ((nCukes, _), (f, t)) => (nCukes.removed(f).updated(t, moveEast), true) }
  }
}