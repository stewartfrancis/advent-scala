package a2021

import scala.collection.mutable

object A15 extends App {
  val lines = util.loadInputLines(getClass)
  val grid = lines.map(_.toCharArray.map(_.asDigit)).toArray

  case class Coord(y: Int, x: Int)

  case class LowestWeightPath(p: List[Coord], w: Int)

  println(lwpToEnd(grid).w)
  val bigGrid = Array(0, 1, 2, 3, 4)
    .flatMap(i => grid.map(y => y.map(x => x + i)))
    .map(arr => Array(0, 1, 2, 3, 4).flatMap(i => arr.map(ai => ((ai + i - 1) % 9) + 1)))

  println(lwpToEnd(bigGrid).w)

  def lwpToEnd(grid: Array[Array[Int]]): LowestWeightPath = {
    val start = Coord(0, 0)
    val queue = mutable.PriorityQueue(LowestWeightPath(List(start), 0))(Ordering.by[LowestWeightPath, Int](_.w).reverse)
    val known = mutable.Set[Coord](start)

    val ys = grid.indices
    val xs = grid(0).indices

    def neighbours(c: Coord): Set[Coord] = {
      Set(Coord(c.y - 1, c.x), Coord(c.y + 1, c.x), Coord(c.y, c.x - 1), Coord(c.y, c.x + 1))
        .filter(c => ys.contains(c.y) && xs.contains(c.x))
    }

    val end = Coord(ys.last, xs.last)
    while (queue.head.p.head != end) {
      val lwp = queue.dequeue()
      val newNeighbours = neighbours(lwp.p.head).diff(known)
      val newLowestWeightPaths = newNeighbours.map(n => LowestWeightPath(n :: lwp.p, lwp.w + grid(n.y)(n.x)))
      known.addAll(newNeighbours)
      queue.addAll(newLowestWeightPaths)
    }
    queue.dequeue()
  }
}
