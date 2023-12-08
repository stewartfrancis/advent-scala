package a2023

import scala.annotation.tailrec

object A4 extends App {
  val lines = util.loadInputLines(getClass).toList

  val rCard = "Card\\s+(\\d+):\\s*(.+)\\|(.+)".r

  //Boring data processing to calculate the number of matches per card
  private val cards = lines
    .map {
      case rCard(cardNum, winning, mine) =>
        def f = (s: String) =>
          //Convert each group of numbers to a set of Int
          s
            .trim
            .split("\\s+")
            .map(_.toInt)
            .toSet
        //return the size of the intersection of the 2 sets
        f(winning).intersect(f(mine)).size
    }

  //Calculate the points 0 if no matches, 2^(matches-1) otherwise
  val points = cards
    .map(s => if (s == 0) 0 else 0x1 << (s - 1))
    .sum

  println(points)

  //Part 2.  Keep a running total of number of copies of all cards in acc and sum at the end
  val totalCards = cards
    .zipWithIndex
    //Fold left with an initial acc representing 1 copy of each card
    .foldLeft(Range(0, cards.size).map(_ => 1))((allCopies, card) => {
      //Get the number of copies of this card
      val copies = allCopies(card._2)
      //Create a new list, overlay on acc with the delta introduced by this card, and return it
      Range(0, cards.size)
        //For each entry, if it's in the range affected by this card's win, set to `copies` otherwise 0
        .map(i => if (i > card._2 && i <= card._2 + card._1) copies else 0)
        //sum with acc by zipping into a tuple and mapping to the summed values
        .zip(allCopies)
        .map(c => c._1 + c._2)
    })
    .sum

  println(totalCards)
}