package a2023

object A6 extends App {
  val lines = util
    .loadInputLines(getClass)
    .toList
    .map(" " + _.substring(9))
    .map(_.grouped(7).map(_.trim.toInt).toList)

  val races = lines.head.zip(lines.tail.head)

  def waysToWin(total: Long, distance: Long): Long = {
    val firstWinningIdx = (0L to total)
      .map(hold => (hold * (total - hold), hold))
      // Drop while we're not winning
      .dropWhile(_._1 <= distance)
      .head
      ._2

    val totalIdxes = total + 1
    (((totalIdxes / 2) - firstWinningIdx) * 2) + (if (totalIdxes % 2 == 0) 0 else 1)
  }

  val p = races
    .map(r => waysToWin(r._1, r._2))
    .product

  println(p)

  println(
    waysToWin(
      races.map(_._1.toString).mkString.toLong,
      races.map(_._2.toString).mkString.toLong
    )
  )
}