package a2023

object A9 extends App {
  val lines = util.loadInputLines(getClass).toList

  val p = lines
    .map(_.split(" ").map(_.toInt).toList)

  def getNext(l: List[Int]): Int = {
    if (!l.exists(_ != 0)) {
      0
    } else {
      val diffs = l
        .zip(l.tail)
        .map { case (a, b) => b - a }
      l.last + getNext(diffs)
    }
  }

  println(p.map(getNext).sum)

  def getPrev(l: List[Int]): Int = {
    if (!l.exists(_ != 0)) {
      0
    } else {
      val diffs = l
        .zip(l.tail)
        .map { case (a, b) => b - a }
      l.head - getPrev(diffs)
    }
  }

  println(p.map(getPrev).sum)
}