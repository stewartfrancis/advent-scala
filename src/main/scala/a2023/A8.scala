package a2023

import scala.annotation.tailrec

object A8 extends App {
  val lines = util.loadInputLines(getClass).toList

  val instructions = lines.head.toList

  case class Node(left: String, right: String)

  val rNode = "(\\w+) = \\((\\w+), (\\w+)\\)".r

  val graph = lines.tail.tail
    .map { case rNode(from, left, right) => (from, Node(left, right)) }
    .toMap

  case class Generator(initial: String) extends Iterator[String] {
    private var prev = initial
    private val i = LazyList
      .from(0)
      .flatMap(_ => instructions)
      .iterator
    override def hasNext: Boolean = !prev.endsWith("Z")

    override def next(): String = {
      prev =
        if ('L'.equals(i.next())) {
          graph(prev).left
        } else {
          graph(prev).right
        }
      prev
    }
  }

  println(Generator("AAA").zipWithIndex.toList.last._2 + 1)

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = a / gcd(a, b) * b

  var lcmPeriod = graph
    .keys
    .filter(_.endsWith("A"))
    .toList
    .map(Generator(_).zipWithIndex.toList.last._2 + 1)
    .map(_.toLong)
    .reduce(lcm)

  println(lcmPeriod)
}