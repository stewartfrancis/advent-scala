package a2021

import scala.annotation.tailrec

object A9 extends App {
  case class Coord(y: Int, x: Int)

  val arr = util.loadInputLines(getClass)
    .map(s => s.toCharArray.map(c => c.asDigit))
    .toArray

  val ys = arr.indices
  val xs = arr(0).indices

  def neighbours(c: Coord) = Set(Coord(c.y - 1, c.x), Coord(c.y + 1, c.x), Coord(c.y, c.x - 1), Coord(c.y, c.x + 1))
    .filter(cc => cc.y >= 0 && cc.x >= 0 && cc.y < ys.end && cc.x < xs.end)

  def height(c: Coord): Int = arr(c.y)(c.x)

  //1
  val lowPoints = ys
    .flatMap(y => xs.map(Coord(y, _))) //coord
    .map(c => (c, neighbours(c).map(height))) //coord, neighbour heights
    .filter { case (c, neighbourHeights) =>
      val h = height(c)
      neighbourHeights.forall(h < _)
    }
    .map {
      _._1
    }

  println(lowPoints.map(height(_) + 1).sum)

  //2 - repeatedly grow each low point until you hit a 9
  def findBasin(l: Coord): Set[Coord] = {
    @tailrec
    def findBasin(basin: Set[Coord], newCoords: Set[Coord]): Set[Coord] = {
      if (newCoords.isEmpty) {
        basin
      } else {
        val newBasin = basin ++ newCoords
        val newNewCoords = newCoords
          .flatMap { n =>
            neighbours(n)
              .filter(!newBasin.contains(_))
              .filter(height(_) != 9)
          }
        findBasin(newBasin, newNewCoords)
      }
    }

    findBasin(Set(), Set(l))
  }

  println(lowPoints.map(findBasin(_).size).sorted.takeRight(3).product)
}
