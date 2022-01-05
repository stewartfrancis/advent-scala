package a2021

object A6 extends App {
  def lines = util.loadInput(getClass).mkString

  val input = lines.split(",").map(Integer.parseInt).toList

  def offsets(n: Int) = {
    Range(0, n)
      .foldLeft(List((1L, 9 * (n + 1)))) { (line, k) => {
        (line.head._1 * (n - k) / (k + 1), 9 * (n - k) + 7 * (k + 1)) :: line
      }
      }
  }

  def countProgeny(daysRemaining: Int) = {
    (List((1L, 0)) :: LazyList
      .from(0)
      .map(offsets)
      .takeWhile(_.head._2 <= daysRemaining)
      .map(_.filter(_._2 <= daysRemaining))
      .toList)
      .map(_.map { x => (1 + (daysRemaining - x._2) / 7) * x._1 }.sum)
      .sum
  }

  def run(daysRemaining: Int) = println(input.size + input.map(x => countProgeny(daysRemaining - x - 1)).sum)

  run(80)
  run(256)
}
