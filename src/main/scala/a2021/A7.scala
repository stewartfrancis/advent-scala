package a2021

import scala.annotation.tailrec

object A7 extends App {

  case class Cell(idx: Int, crabsLeft: Int, crabsRight: Int)

  def crabs =
    util.loadInputLines(getClass).next()
      //    "16,1,2,0,4,2,7,1,2,14"
      .split(",").map(Integer.parseInt).sorted.toList

  def crabz(crabs: List[Int], startDistance: Int, totalCrabs: Int): (Int, Int) = {
    @tailrec
    def icrabz(crabs: List[Int], pos: Int, seenCrabs: Int, unseenCrabs: Int, score: Int): (Int, Int) = crabs match {
      case ::(crab, next) =>
        if (crab == pos) {
          icrabz(next, pos, seenCrabs + 1, unseenCrabs - 1, score) //stay at the same pos until we see a future crab
        } else { //Don't consume the crab, (current pos gets its final score, and we can compare it to best?)
          def nextScore = score + seenCrabs - unseenCrabs

          if (nextScore > score) {
            (pos, score)
          } else {
            icrabz(crab :: next, pos + 1, seenCrabs, unseenCrabs, nextScore) //move to the next position
          }
        }
      case Nil => (pos, score)
    }

    icrabz(crabs, 0, 0, totalCrabs, startDistance)
  }

  def totalCrabs = crabs.length

  def startDistance1 = crabs.sum

  println(crabz(crabs, startDistance1, totalCrabs)._2)

  def crabz2(crabs: List[Int]): (Int, Int) = {
    @tailrec
    def icrabz(crabs: List[Int], pos: Int, seenCrabs: Int, unseenCrabs: Int, score: Int, prevIncrement: Int, decrement: Int): (Int, Int) = crabs match {
      case ::(crab, next) =>
        if (crab == pos) {
          icrabz(next, pos, seenCrabs + 1, unseenCrabs - 1, score, prevIncrement, decrement) //stay at the same pos until we see a future crab
        } else { //Don't consume the crab, but we know the next score so can check it
          val increment = prevIncrement + seenCrabs

          val nextScore = score + increment - decrement

          if (nextScore > score) {
            (pos, score)
          } else {
            icrabz(crab :: next, pos + 1, seenCrabs, unseenCrabs, nextScore, increment, decrement - unseenCrabs) //move to the next position
          }
        }
      case Nil => (pos, score)
    }

    val startScore = crabs.map(n => (n * (n + 1)) / 2).sum

    val startDecrement = crabs.sum

    icrabz(crabs, 0, 0, totalCrabs, startScore, 0, startDecrement)
  }

  println(crabz2(crabs)._2)
}
