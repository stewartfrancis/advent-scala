package a2021

object A5 extends App {

  case class Coord(x: Int, y: Int)

  case class Line(f: Coord, t: Coord) {
    def points(): Seq[Coord] = {
      def xRange = Range.inclusive(f.x, t.x, if (f.x < t.x) 1 else -1)

      def yRange = Range.inclusive(f.y, t.y, if (f.y < t.y) 1 else -1)

      if (f == t) {
        Seq(f)
      } else if (f.x == t.x) {
        yRange.map(Coord(f.x, _))
      } else if (f.y == t.y) {
        xRange.map(Coord(_, f.y))
      } else {
        xRange.zip(yRange).map(t => Coord(t._1, t._2))
      }
    }
  }

  val m = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

  def lines = util.loadInputLines(getClass).toList
    .map { case m(fx, fy, tx, ty) => Line(
      Coord(Integer.valueOf(fx), Integer.valueOf(fy)),
      Coord(Integer.valueOf(tx), Integer.valueOf(ty)))
    }

  def countOverlaps(lines: Seq[Line]) = lines
    .flatMap(_.points())
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .count(_._2 > 1)

  println(countOverlaps(lines.filter(l => l.f.x == l.t.x || l.f.y == l.t.y)))
  println(countOverlaps(lines))
}
