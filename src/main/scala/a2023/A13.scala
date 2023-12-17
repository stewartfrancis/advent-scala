package a2023

import util.{Coord, CoordMap}

object A13 extends App {

  val maps = util
    .loadInputLines(getClass)
    .mkString("\n")
    .split("\n\n")
    .map(m => util.toCoordMap(m.split("\n").toList)).toList

  def search(m: CoordMap, glitches: Boolean): Int = {
    searchM(m, horizontal = true, glitches)
      .map(_ * 100)
      .orElse(
        searchM(m, horizontal = false, glitches)
      )
      .getOrElse(0)
  }

  def searchM(m: CoordMap, horizontal: Boolean, glitches: Boolean): Option[Int] = {
    val getTarget: Coord => Int = if (horizontal) c => c.r else c => c.c
    val coordFactory: (Int, Coord) => Coord = if (horizontal) (i, c) => Coord(i, c.c) else (i, c) => Coord(c.r, i)
    val bound = if (horizontal) m.rows else m.cols
    Range.inclusive(1, bound - 1)
      .find(i => {
        val nearestBoundDistance = i.min(bound - i)
        m.coords
          .filter(c => {
            val target = getTarget(c._1)
            target < i + nearestBoundDistance && target >= i - nearestBoundDistance
          })
          .partition(c => getTarget(c._1) < i) match {
          //now have equally sized l and r.  Map the l coordinates into the r space so they'll be equal if a mirror image
          case (l, r) =>
            val x = l
              .map(c => {
                val target = getTarget(c._1)
                val newTarget = target + ((i - target) * 2) - 1
                (coordFactory(newTarget, c._1), c._2)
              })
            if (glitches) {
              x.toSet.diff(r.toSet).size == 1
            } else {
              x.equals(r)
            }
        }
      })
  }

  val p1 = maps.map(search(_, glitches = false)).sum

  println(p1)

  val p2 = maps.map(search(_,  glitches = true)).sum

  println(p2)

}