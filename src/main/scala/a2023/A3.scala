package a2023

object A3 extends App {
  val lines = util.loadInputLines(getClass).toList

  //Create a list of all symbols paired with a set of their adjacent coords
  val symbols = lines
    //double zip and flat map to get every char and its coords
    .map(_.zipWithIndex.toList)
    .zipWithIndex
    .flatMap(r => r._1.map(s => (s._1, r._2, s._2)))
    //ignore non-symbols
    .filter(c => !c._1.isDigit && c._1 != '.')
    //Expand coord for each symbol to the surrounding coords too
    .map(c => (
      c._1,
      (for {
          r <- Range.inclusive(c._2 - 1, c._2 + 1)
          c <- Range.inclusive(c._3 - 1, c._3 + 1)
        } yield (r, c)
      ).toSet
    ))

  //Ugly regex to find numbers preceded and succeeded by a non-digit
  val number = "\\D(\\d+)\\D?.?".r

  //Create a list of all numbers and the coords their digits occupy
  val numbers = lines
    .zipWithIndex
    .flatMap(l => (" " + l._1 + "   ")
      .sliding(4)
      .zipWithIndex
      .flatMap {
        case (s, i) => s match {
          case number(n) => Some((
            n.toInt,
            Range(i, i + n.length).map((l._2, _)).toSet))
          case _ => None
        }
      }
      .toList
    )

  //Get a list of any coord that is adjacent to a symbol
  val partAdjacentCoords = symbols
    .flatMap(_._2)
    .toSet

  //Find all numbers whose coords intersect with the set of part adjacent coords, sum their values
  val partsSum = numbers
    .filter(_._2.intersect(partAdjacentCoords).nonEmpty)
    .map(_._1)
    .sum

  println(partsSum)

  //Similar again, first filter symbols to only include gears
  val gearRatios = symbols
    .filter(_._1 == '*')
    .map(_._2)
    .map(g => {
      //For each gear, find the surrounding numbers
      def ns = numbers.filter(n => n._2.intersect(g).nonEmpty)

      if (ns.size == 2) {
        //If there are 2 adjacent numbers, return their product
        ns.map(_._1).product
      } else {
        //Else return 0
        0
      }
    })
    //Sum the products
    .sum

  println(gearRatios)
}