package a2021

object A1 extends App {
  def lines = util.loadInputLines(getClass).toList.map(Integer.parseInt)

  println(lines.zip(lines.tail).count(t => t._2 > t._1))
  println(lines.zip(lines.slice(3, lines.size)).count(t => t._2 > t._1))
}
