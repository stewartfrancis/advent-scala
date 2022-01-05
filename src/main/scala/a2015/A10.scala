package a2015

import scala.annotation.tailrec

object A10 extends App {

  def lookSay(digits: List[Int]): List[Int] = {
    @tailrec
    def lookSayAcc(digits: List[Int], acc: List[Int]): List[Int] = {
      digits match {
        case Nil => acc
        case ::(head, next) =>
          next.span(_ == head) match {
            case (repeats, rest) =>
              lookSayAcc(rest, (1 + repeats.size) :: head :: acc)
          }
      }
    }

    lookSayAcc(digits.reverse, Nil)
  }

  val i = Iterator
    .iterate(1113122113.toString.map(_.asDigit).toList)(lookSay)

  println(i.drop(50).next.size)

}
