package a2023

import scala.collection.immutable.ListMap

object A7 extends App {

  val lines = util
    .loadInputLines(getClass)
    .map(l => (l.substring(0, 5), l.substring(5).trim.toInt))
    .toList

  private def count(hand: String) = ListMap(
    hand
      .toList
      .groupBy(identity)
      .map { case (c, l) => (c, l.size) }
      //Sorted by number of occurrences descending
      .toSeq
      .sortBy(_._2)(Ordering.Int.reverse):_*
  )

  object Part1Ordering extends Ordering[String] {
    private def replaceChars(s: String): String = {
      s.replace('A', 'Z')
        .replace('K', 'Y')
        .replace('Q', 'X')
        .replace('J', 'W')
        .replace('T', 'V')
    }

    override def compare(x: String, y: String): Int = {
      val cx = count(x)
      val cy = count(y)

      Option(cx.head._2 - cy.head._2)
        .filter(_ != 0)
        .orElse(Some(cx.tail.head._2 - cy.tail.head._2))
        .filter(_ != 0)
        .getOrElse(replaceChars(x).compareTo(replaceChars(y)))
    }
  }

  def score(o: Ordering[String]) = lines
    .sortBy(_._1)(o)
    .zipWithIndex
    .map(t => t._1._2 * (t._2 + 1))
    .sum

  println(score(Part1Ordering))

  object Part2Ordering extends Ordering[String] {
    private def replaceChars(s: String): String = {
      s.replace('A', 'Z')
        .replace('K', 'Y')
        .replace('Q', 'X')
        //Make J lexically weak
        .replace('J', '0')
        .replace('T', 'V')
    }

    override def compare(x: String, y: String): Int = {
      val cx = count(x)
      val cy = count(y)
      val jx = cx.getOrElse('J', 0)
      val jy = cy.getOrElse('J', 0)
      val fx = cx.filter(_._1 != 'J')
      val fy = cy.filter(_._1 != 'J')
      val fx1 = if (fx.isEmpty) 0 else fx.head._2
      val fy1 = if (fy.isEmpty) 0 else fy.head._2
      val fx2 = if (fx.isEmpty || fx.tail.isEmpty) 0 else fx.tail.head._2
      val fy2 = if (fy.isEmpty || fy.tail.isEmpty) 0 else fy.tail.head._2
      Option(fx1 + jx - fy1 - jy)
        .filter(_ != 0)
        .orElse(Some(fx2 - fy2))
        .filter(_ != 0)
        .getOrElse(replaceChars(x).compareTo(replaceChars(y)))
    }
  }

  println(score(Part2Ordering))
}
