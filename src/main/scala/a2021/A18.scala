package a2021

import scala.annotation.tailrec

object A18 extends App {
  val lines = util.loadInputLines(getClass).map(parse).toList

  class Node()

  case class Val(i: Int) extends Node

  def explode(n: (Any, Any)) = n match {
    case (((((Val(_), Val(r)), a), b), c), d) => Some(((((Val(0), addL(r, a)), b), c), d)) //llll
    case ((((a, (Val(l), Val(r))), b), c), d) => Some(((((addR(l, a), Val(0)), addL(r, b)), c), d)) //lllr
    case (((a, ((Val(l), Val(r)), b)), c), d) => Some((((addR(l, a), (Val(0), addL(r, b))), c), d)) //llrl
    case (((a, (b, (Val(l), Val(r)))), c), d) => Some((((a, (addR(l, b), Val(0))), addL(r, c)), d)) //llrr
    case ((a, (((Val(l), Val(r)), b), c)), d) => Some(((addR(l, a), ((Val(0), addL(r, b)), c)), d)) //lrll
    case ((a, ((b, (Val(l), Val(r))), c)), d) => Some(((a, ((addR(l, b), Val(0)), addL(r, c))), d)) //lrlr
    case ((a, (b, ((Val(l), Val(r)), c))), d) => Some(((a, (addR(l, b), (Val(0), addL(r, c)))), d)) //lrrl
    case ((a, (b, (c, (Val(l), Val(r))))), d) => Some(((a, (b, (addR(l, c), Val(0)))), addL(r, d))) //lrrr
    case (a, ((((Val(l), Val(r)), b), c), d)) => Some((addR(l, a), (((Val(0), addL(r, b)), c), d))) //rlll
    case (a, (((b, (Val(l), Val(r))), c), d)) => Some((a, (((addR(l, b), Val(0)), addL(r, c)), d))) //rllr
    case (a, ((b, ((Val(l), Val(r)), c)), d)) => Some((a, ((addR(l, b), (Val(0), addL(r, c))), d))) //rlrl
    case (a, ((b, (c, (Val(l), Val(r)))), d)) => Some((a, ((b, (addR(l, c), Val(0))), addL(r, d)))) //rlrr
    case (a, (b, (((Val(l), Val(r)), c), d))) => Some((a, (addR(l, b), ((Val(0), addL(r, c)), d)))) //rrll
    case (a, (b, ((c, (Val(l), Val(r))), d))) => Some((a, (b, ((addR(l, c), Val(0)), addL(r, d))))) //rrlr
    case (a, (b, (c, ((Val(l), Val(r)), d)))) => Some((a, (b, (addR(l, c), (Val(0), addL(r, d)))))) //rrrl
    case (a, (b, (c, (d, (Val(l), Val(r)))))) => Some((a, (b, (c, (addR(l, d), Val(0)))))) //rrrr
    case _ => None
  }

  def addL(i: Int, n: Any): Any = n match {
    case Val(x) => Val(x + i)
    case (l, r) => (addL(i, l), r)
  }

  def addR(i: Int, n: Any): Any = n match {
    case Val(x) => Val(x + i)
    case (l, r) => (l, addR(i, r))
  }

  def split(n: Any): Option[(Any, Any)] = n match {
    case Val(x) => Option(x).filter(_ > 9).map(x => (Val(x / 2), Val((x / 2) + (x % 2))))
    case (l, r) => split(l).map((_, r)).orElse(split(r).map((l, _)))
  }

  @tailrec
  def reduce(p: (Any, Any)): (Any, Any) = {
    explode(p).orElse(split(p)) match {
      case Some(p) => reduce(p)
      case None => p
    }
  }

  def add(p1: (Any, Any), p2: (Any, Any)) = reduce((p1, p2))

  def magnitude(n: Any): Int = n match {
    case Val(x) => x
    case (l, r) => (3 * magnitude(l)) + (2 * magnitude(r))
  }

  def parse(s: String): (Any, Any) = {
    @tailrec
    def parse(chars: List[Char], nodes: List[Any]): (Any, Any) = {
      chars match {
        case head :: tail => head match {
          case x if x.isDigit => parse(tail, Val(x.asDigit) :: nodes)
          case ']' => nodes match {
            case head :: next :: rest => parse(tail, (next, head) :: rest)
          }
          case _ => parse(tail, nodes)
        }
        case Nil => nodes.head.asInstanceOf[(Any, Any)]
      }
    }

    parse(s.toCharArray.toList, Nil)
  }
  //1
  println(magnitude(lines.reduce(add)))
  //2
  println(lines.combinations(2).map(t => magnitude(add(t.head, t.tail.head))).max)
}
