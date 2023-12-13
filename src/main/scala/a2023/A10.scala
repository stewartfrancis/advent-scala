package a2023

import scala.collection.immutable.ListMap

object A10 extends App {
  val lines = util.loadInputLines(getClass).toList

  //Convert the input into an coordinate space and put it into a map
  //(x, y) -> value
  val map = ListMap(lines
    .map(_.toList.zipWithIndex)
    .zipWithIndex
    .flatMap(c => c._1.map(r => (Coord(c._2, r._2), r._1)))
    : _*)

  case class Coord(r: Int, c: Int) {
    //return coords of a relative location mapped by an int representing N to NW
    def move(dir: Int): Coord = dir match {
      case 0 => Coord(r - 1, c)
      case 1 => Coord(r - 1, c + 1)
      case 2 => Coord(r, c + 1)
      case 3 => Coord(r + 1, c + 1)
      case 4 => Coord(r + 1, c)
      case 5 => Coord (r + 1, c - 1)
      case 6 => Coord(r, c - 1)
      case 7 => Coord(r - 1, c - 1)
    }

    //get a list of neighbouring coordinates described by a range modulo 8
    def neighbours(from: Int, to: Int): List[Coord] = {
      Range.inclusive(from, to).map(i => move(i % 8)).toList
    }

    //get the value of this coordinate
    def value: Char = map(this)
  }

  //walk a path from a supplied start location, and the direction of travel to the next coordinate
  case class PathIterator(initial: Coord, dir: Int) extends Iterator[(Coord, List[Coord], List[Coord])] {
    private var n: (Coord, (Int, List[Coord], List[Coord])) = (initial, (dir, List(), List()))
    var started = false

    override def hasNext: Boolean = !started || n._1.value != 'S'

    //return a tuple of (coordinates of the next path square, coordinates of the cells on the left, and coords on the right
    def next(prev: (Coord, Int)): (Coord, (Int, List[Coord], List[Coord])) = {
      val c = prev._1
      val d = prev._2
      val nextDir: (Int, List[Coord], List[Coord]) = c.value match {
        case 'L' =>
          val a = c.neighbours(1, 1)
          val b = c.neighbours(3, 7)
          if (d == 4) (2, a, b) else (0, b, a)
        case 'J' =>
          val a = c.neighbours(1, 5)
          val b = c.neighbours(7, 7)
          if (d == 4) (6, a, b) else (0, b, a)
        case '7' =>
          val a = c.neighbours(7, 11)
          val b = c.neighbours(5, 5)
          if (d == 2) (4, a, b) else (6, b, a)
        case 'F' =>
          val a = c.neighbours(5, 9)
          val b = c.neighbours(3, 3)
          if (d == 0) (2, a, b) else (4, b, a)
        case '-' =>
          val a = c.neighbours(7, 9)
          val b = c.neighbours(3, 5)
          if (d == 2) (d, a, b) else (d, b, a)
        case '|' =>
          val a = c.neighbours(1, 3)
          val b = c.neighbours(5, 7)
          if (d == 4) (d, a, b) else (d, b, a)
        case 'S' => (d, List(), List())
      }
      (c.move(nextDir._1), nextDir)
    }

    override def next(): (Coord, List[Coord], List[Coord]) = {
      val r = n._1
      n = next(n._1, n._2._1)
      started = true
      (r, n._2._2, n._2._3)
    }
  }

  //Find the start
  val s = map.filter(_._2 == 'S').head
  val path = PathIterator(s._1, 0).toList //hard-coded for my input to move N

  //Answer to part 1
  println(path.length / 2)

  //Get all the coordinates of the path entries
  val pathCoords = path.map(_._1).toSet

  //Get all coordinates on the right hand side (relative to dir of travel), exclude anything on the path
  var inside = path.flatMap(_._3).toSet.filter(!pathCoords.contains(_))

  //Flood fill gaps, by iteratively adding unvisited neighbours to the inside
  var prevSize = 0
  var newSize = inside.size
  //Stop adding when an iteration doesn't find any new coords
  while (newSize != prevSize) {
    val insideNeighbours = inside.flatMap(_.neighbours(0, 7)).filter(!pathCoords.contains(_))
    inside = inside ++ insideNeighbours
    prevSize = newSize
    newSize = inside.size
  }

  println(newSize)
}