package a2023

import scala.annotation.tailrec

object A8 extends App {
  val lines = util.loadInputLines(getClass).toList

  val instructions = lines.head.toList

  def infiniteInstructions() = LazyList
    .from(0)
    .flatMap(_ => instructions)
    .iterator

  def nextNode(prev: String, c: Char) = if ('L'.equals(c)) graph(prev).left else graph(prev).right

  case class Node(left: String, right: String)

  val rNode = "(\\w+) = \\((\\w+), (\\w+)\\)".r

  val graph = lines.tail.tail
    .map { case rNode(from, left, right) => (from, Node(left, right)) }
    .toMap

  case class Generator(initial: String) extends Iterator[String] {
    private var prev = initial
    private val i = infiniteInstructions()

    override def hasNext = true

    override def next(): String = {
      prev = nextNode(prev, i.next())
      prev
    }
  }

  var periods = graph
    .keys
    .filter(_.endsWith("A"))
    .toList
    .map(Generator(_).zipWithIndex)
    .map(_.dropWhile(i => !i._1.endsWith("Z")).next()._2 + 1)

  @tailrec
  def gcd(aa: Long, bb: Long): Long = if (bb == 0) aa else gcd(bb, aa % bb)

  def lcm(a: Long, b: Long): Long = a / gcd(a, b) * b

  println(periods.map(_.toLong).reduce(lcm))
}