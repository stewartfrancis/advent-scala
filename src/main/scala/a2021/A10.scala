package a2021

import scala.annotation.tailrec

object A10 extends App {
  val inputs = util.loadInputLines(getClass).map(_.toList)

  @tailrec
  def parse(input: List[Char], context: List[Char]): Either[Int, List[Char]] = input match {
    case ::(head, next) => head match {
      case '{' => parse(next, '}' :: context)
      case '<' => parse(next, '>' :: context)
      case '(' => parse(next, ')' :: context)
      case '[' => parse(next, ']' :: context)
      case x if context.head == x => parse(next, context.tail)
      case ')' => Left(3)
      case ']' => Left(57)
      case '}' => Left(1197)
      case _ => Left(25137)
    }
    case Nil => Right(context)
  }

  val parsed = inputs.map(parse(_, List())).toList

  //1
  println(parsed.map(_.left.getOrElse(0)).sum)

  //2
  @tailrec
  def complete(context: List[Char], score: Long): Long = context match {
    case ::(head, next) =>
      val inc = head match {
        case ')' => 1
        case ']' => 2
        case '}' => 3
        case _ => 4
      }
      complete(next, (score * 5) + inc)
    case Nil => score
  }

  val completeScores = parsed
    .map(_.toOption)
    .filter(_.nonEmpty)
    .map(_.get)
    .map(complete(_, 0))
    .sorted

  println(completeScores(completeScores.size / 2))
}
