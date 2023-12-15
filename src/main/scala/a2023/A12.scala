package a2023

object A12 extends App {

  val lines = util.loadInputLines(getClass).toList

  val r = "(.*) (.*)".r

  val memo = scala.collection.mutable.Map[(List[Char], List[Int], Int), Long]()

  def countMatches(pattern: List[Char], numbers: List[Int], currentRange: Int): Long = {
    val key = (pattern, numbers, currentRange)
    if (memo.contains(key)) {
      memo(key)
    } else {
      val r = countMatchesM(pattern, numbers, currentRange)
      memo.put(key, r)
      r
    }
  }

  def countMatchesM(pattern: List[Char], numbers: List[Int], currentRange: Int): Long = {
    pattern match {
      case '#' :: tail =>
        if (currentRange == 0) {
          //try and start a new range
          if (numbers.nonEmpty) {
            countMatches(tail, numbers, 1)
          } else {
            //illegal start of a new range, no more matches possible here
            0L
          }
        } else {
          //add to existing range
          if (numbers.head > currentRange) {
            countMatches(tail, numbers, currentRange + 1)
          } else {
            //range becomes too long, no more matches possible here
            0L
          }
        }
      //Unknown char, try both!
      case '?' :: tail =>
        countMatches('#' :: tail, numbers, currentRange) +
        countMatches('.' :: tail, numbers, currentRange)
      case '.' :: tail =>
        if (currentRange > 0) {
          if (currentRange == numbers.head) {
            //legal end of a range
            countMatches(tail, numbers.tail, 0)
          } else {
            //illegal end of a range, no more matches possible here
            0L
          }
        } else {
          countMatches(tail, numbers, 0)
        }
      case Nil =>
        if (currentRange > 0) {
          if (numbers.nonEmpty && currentRange == numbers.head && numbers.tail.isEmpty) {
            1L
          } else {
            0L
          }
        } else if (numbers.isEmpty) {
          1L
        } else {
          0L
        }
    }
  }

  def countMatches(pattern: String, numbers: String): Long = {
    countMatches(pattern.toList, numbers.split(",").map(_.toInt).toList, 0)
  }

  val part1 = lines
    .map { case r(pattern, numbers) => countMatches(pattern, numbers) }
    .sum

  println(part1)

  val part2 = lines
    .map { case r(pattern, numbers) =>
      val newP = pattern + "?" + pattern + "?" + pattern + "?" + pattern + "?" + pattern
      val newN = numbers + "," + numbers + "," + numbers + "," + numbers + "," + numbers
      countMatches(newP, newN)
    }
    .sum

  println(part2)
}