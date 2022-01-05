package a2015

import scala.annotation.tailrec

object A11 extends App {
  val input = "cqjxjnds"
    .map(_ - 97)
    .toList

  val i = ('i' - 97)
  val o = ('o' - 97)
  val l = ('l' - 97)

  def inc(pw: List[Int]): (Int, List[Int]) = {
    pw match {
      case ::(head, next) =>
        val rest = inc(next)
        val incd = head + rest._1
        (incd / 26, incd % 26 :: rest._2)
      case Nil => (1, Nil)
    }
  }

  def invalid(pw: List[Int]): Boolean = {
    def isIllegalDigit(digit: Int): Boolean = {
      digit == i || digit == o || digit == l
    }

    def updatePairs(seen: Set[Int], prev: Int, next: Int): Set[Int] = {
      if (prev == next && !seen.contains(next)) {
        seen + next
      } else {
        seen
      }
    }

    @tailrec
    def invalidAcc(pw: List[Int], seenRun: Boolean, pairs: Set[Int]): Boolean = {
      pw match {
        case prev2 :: prev1 :: next :: rest =>
          isIllegalDigit(next) || invalidAcc(
            prev1 :: next :: rest,
            seenRun || (prev2 == next - 2 && prev1 == next - 1),
            updatePairs(pairs, prev1, next)
          )
        case prev1 :: next :: rest =>
          isIllegalDigit(next) || invalidAcc(next :: rest, seenRun, updatePairs(pairs, prev1, next))
        case ::(next, rest) => isIllegalDigit(next) || invalidAcc(rest, seenRun, pairs)
        case Nil => !seenRun || pairs.size < 2
      }
    }

    invalidAcc(pw, seenRun = false, Set.empty[Int])
  }

  private val it = Iterator.iterate(input)(inc(_)._2)

  def decodePw(pw: List[Int]) = pw.map(_ + 97).map(_.toChar).mkString

  println(decodePw(it.dropWhile(invalid).next()))
  println(decodePw(it.dropWhile(invalid).next()))

}
