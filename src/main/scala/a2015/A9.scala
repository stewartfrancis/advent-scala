package a2015

import scala.io.Source

object A9 extends App {
  val m = "(\\w+) to (\\w+) = (\\d+)".r

  //Parse distances
  val parsed = Source
    .fromInputStream(getClass.getResourceAsStream("a2015/A9.txt"))
    .getLines
    .toList
    .map { case m(a, b, dist) => (a, b, dist.toInt) }

  //Make matrix of distances
  val distances = parsed
    .foldLeft(Map[String, Map[String, Int]]().withDefaultValue(Map())) { (acc, e) => {
      val dest1 = acc(e._1) + (e._2 -> e._3)
      val dest2 = acc(e._2) + (e._1 -> e._3)
      acc + (e._1 -> dest1) + (e._2 -> dest2)
    }
    }

  def findShortest(origin: String, shortest: Int, soFar: Int, toVisit: Set[String]): Int = {
    if (soFar >= shortest) {
      shortest
    } else if (toVisit.isEmpty) {
      soFar
    } else {
      toVisit.foldLeft(shortest) { (shortest, next) => findShortest(next, shortest, soFar + distances(origin)(next), toVisit - next) }
    }
  }

  //For each place, try visiting all the other places until there's nowhere left
  val shortest = distances
    .foldLeft(Int.MaxValue) { case (shortest, (origin, destinations)) =>
      findShortest(origin, shortest, 0, destinations.keys.toSet)
    }

  println(shortest)

  def findLongest(origin: String, longest: Int, soFar: Int, toVisit: Set[String]): Int = {
    if (toVisit.isEmpty) {
      if (soFar > longest) soFar else longest
    } else {
      toVisit.foldLeft(longest) { (longest, next) => findLongest(next, longest, soFar + distances(origin)(next), toVisit - next) }
    }
  }

  //For each place, try visiting all the other places until there's nowhere left
  val longest = distances
    .foldLeft(0) { case (longest, (origin, destinations)) =>
      findLongest(origin, longest, 0, destinations.keys.toSet)
    }

  println(longest)
}
