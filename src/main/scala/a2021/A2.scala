package a2021

object A2 extends App {
  def lines = util.loadInputLines(getClass)
    .toList
    .map(l => l.split("\\s"))
    .map(s => (s(0), Integer.parseInt(s(1))))

  Option(lines)
    .map(o => o.foldLeft((0, 0)) { (acc, t) =>
      t match {
        case ("forward", i) => (acc._1 + i, acc._2)
        case ("down", i) => (acc._1, acc._2 + i)
        case ("up", i) => (acc._1, acc._2 - i)
      }
    }
    )
    .map(p => p._1 * p._2)
    .foreach(println)

  Option(lines)
    .map(o => o.foldLeft((0, 0, 0)) { (acc, t) =>
      t match {
        case ("forward", i) => (acc._1 + i, acc._2 + (acc._3 * i), acc._3)
        case ("down", i) => (acc._1, acc._2, acc._3 + i)
        case ("up", i) => (acc._1, acc._2, acc._3 - i)
      }
    }
    )
    .map(p => p._1 * p._2)
    .foreach(println)
}
