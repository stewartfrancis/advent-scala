package a2021

object A20 extends App {
  val lines = util.loadInputLines(getClass)

  case class Grid(g: Array[Array[Int]], infiniteSpace: Int) {

    def generate(): Grid = {
      val newGrid = (-2 until g.length)
        .map(y => (-2 until g.head.length)
          .map { x =>
            val index = (lookUp(y, x) << 8) | (lookUp(y, x + 1) << 7) | (lookUp(y, x + 2) << 6) |
              (lookUp(y + 1, x) << 5) | (lookUp(y + 1, x + 1) << 4) | (lookUp(y + 1, x + 2) << 3) |
              (lookUp(y + 2, x) << 2) | (lookUp(y + 2, x + 1) << 1) | lookUp(y + 2, x + 2)
            alg(index)
          }
          .toArray
        )
        .toArray

      Grid(newGrid, if (infiniteSpace == 1) alg.last else alg.head)
    }

    def lookUp(y: Int, x: Int): Int = {
      if (x < 0 || x >= g.head.length || y < 0 || y >= g.length) {
        infiniteSpace
      } else {
        g(y)(x)
      }
    }

    def countLit(): Int = g.flatten.count(_ == 1)

    override def toString: String = g.map(r => r.map(i => if (i == 1) '#' else '.').mkString).mkString("\n")
  }

  def readLine(s: String) = s.map(c => if (c == '#') 1 else 0).toArray

  val alg = readLine(lines.next())
  lines.drop(1)
  val grid = Grid(lines.map(readLine).toArray, 0)

  val iter2 = grid.generate().generate()
  //1
  println(iter2.countLit())
  //2
  println((0 until 48).foldLeft(iter2)((g, _) => g.generate()).countLit())
}
