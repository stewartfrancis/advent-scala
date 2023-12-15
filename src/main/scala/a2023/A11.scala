package a2023

import util.Coord

object A11 extends App {

  val lines = util.loadInputLines(getClass).toList

  val emptyRowIndicies = lines
    .zipWithIndex
    .filter(_._1.forall('.'.equals))
    .map(_._2)

  val emptyColIndicies = util.toCoords(lines)
    .groupBy(_._1.c)
    .filter(_._2.forall(_._2.equals('.')))
    .keys

  val map = util.toCoords(lines)
    .filter(_._2 == '#')
    .keys
    .toList

  def sumDistances(map: List[Coord], factor: Int): Long = {
    (for {x <- map; y <- map} yield (x, y))
      .filter(i => i._1 != i._2)
      .map(i => {
        val smallRow = i._1.r.min(i._2.r).toLong
        val bigRow = i._1.r.max(i._2.r).toLong
        val emptyRowsCrossed = emptyRowIndicies.count(r => r >= smallRow && r <= bigRow)
        val smallCol = i._1.c.min(i._2.c).toLong
        val bigCol = i._1.c.max(i._2.c).toLong
        val emptyColsCrossed = emptyColIndicies.count(r => r >= smallCol && r <= bigCol)
        (bigRow - smallRow) + (emptyRowsCrossed * (factor - 1)) + (bigCol - smallCol) + (emptyColsCrossed * (factor - 1))
      })
      .sum / 2
  }

  println(sumDistances(map, 2))
  println(sumDistances(map, 1000000))
}