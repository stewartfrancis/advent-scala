package a2021

object A13 extends App {
  val lines = util.loadInputLines(getClass)

  case class Coord(x: Int, y: Int)

  case class Fold(axis: String, index: Int) {
    def fold(coords: Set[Coord]): Set[Coord] = {
      if (axis == "x") {
        val (before, after) = coords.partition(c => c.x < index)
        before ++ after.map(c => Coord((index * 2) - c.x, c.y))
      } else {
        val (before, after) = coords.partition(c => c.y < index)
        before ++ after.map { c => Coord(c.x, (index * 2) - c.y) }
      }
    }
  }

  val r1 = "([^,]*?),(.*)".r

  val coords = lines.takeWhile(r1.matches).map { case r1(x, y) => Coord(x.toInt, y.toInt) }.toSet

  lines.take(1) //blank line
  val r2 = "fold along (y|x)=(.*)".r

  val finalCoords = lines
    .map { case r2(axis, index) => Fold(axis, index.toInt) }
    .foldLeft(coords) { (coords, f) => f.fold(coords) }

  val maxX = finalCoords.map(_.x).reduce(Math.max)
  val maxY = finalCoords.map(_.y).reduce(Math.max)
  val out = Array.fill(maxY + 1, maxX + 1)(' ')
  finalCoords.foreach(c => out(c.y)(c.x) = 'X')

  out
    .map(_.mkString)
    .foreach(println)
}
