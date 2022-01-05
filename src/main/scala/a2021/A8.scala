package a2021

object A8 extends App {

  val r1 = "(.*?)\\|(.*?)".r

  val input = util.loadInputLines(getClass)
    .map { case r1(l, r) => (
      l.trim.split(" ").toList.map(_.toCharArray.toSet),
      r.trim.split(" ").toList.map(_.toCharArray.toSet)
    )}
    .toList

  //1
  println(input.map { l => l._2.count(d => Set(2, 3, 4, 7).contains(d.size)) }.sum)

  //2
  println(input.map { case (in, out) => solve(in, out) }.sum)

  //map letters to potential letters
  def solve(in: List[Set[Char]], out: List[Set[Char]]) = {
    val digitsByLength = in.groupBy(_.size)
    val d1 = digitsByLength(2).head
    val d7 = digitsByLength(3).head
    val d8 = digitsByLength(7).head
    val d4 = digitsByLength(4).head
    val d3 = digitsByLength(5).filter(d => d7.diff(d).isEmpty).head
    val d9 = digitsByLength(6).filter(d => d4.diff(d).isEmpty).head
    val d5 = digitsByLength(5).diff(List(d3)).filter(d => d.diff(d9).isEmpty).head
    val d2 = digitsByLength(5).diff(List(d3, d5)).head
    val d6 = digitsByLength(6).filter(d => d8.diff(d7).diff(d).isEmpty).head
    val d0 = digitsByLength(6).diff(List(d6, d9)).head

    val segsToDigits = List(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9).zipWithIndex.toMap
    out.map(d => segsToDigits(d)) match {
      case a :: b :: c :: d :: Nil => (1000 * a) + (100 * b) + (10 * c) + d
    }
  }
}
