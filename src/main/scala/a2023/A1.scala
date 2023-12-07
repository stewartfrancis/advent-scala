package a2023

object A1 extends App {

  def lines = util.loadInputLines(getClass).toList

  def p1 = lines
    //filter p1 map numeric chars
    .map(s => s.toList.filter(Character.isDigit))
    //concat the head and the last
    .map(l => ("" + l.head + l.last).toInt)
    .sum

  println(p1)

  //build a map of the word to its char representation for later
  val words = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    .zipWithIndex
    .map { case (w, i) => (w, (i + 49).toChar) }

  def p2 = lines
    .map(l =>
      //pad with spaces so we get the last characters
      (l + "    ")
      //now we can take a sliding window of 5 chars, and the interesting bit will be at the start
      .sliding(5)
      .flatMap(s => Option(s.head)
        //either it's a digit
        .filter(_.isDigit)
        .orElse(
          //or if it starts with a word representing a digit, take the corresponding value from the map
          words
            .flatMap { case (w, v) => Option(v).filter(_ => s.startsWith(w)) }
            //or its not interesting
            .headOption
        )
      )
      .toList
    )
    .map(l => ("" + l.head + l.last).toInt)
    .sum

  println(p2)
}