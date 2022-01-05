package a2021

object A11 extends App {
  case class Coord(y: Int, x: Int)

  case class Cell(i: Int, n: Set[Int])

  val arr = util.loadInputLines(getClass)
    .map(s => s.toCharArray.map(c => c.asDigit))
    .toArray

  val ys = arr.indices
  val xs = arr(0).indices

  def neighbours(c: Coord) =
    Set(
      Coord(c.y - 1, c.x - 1), Coord(c.y - 1, c.x), Coord(c.y - 1, c.x + 1),
      Coord(c.y, c.x - 1), Coord(c.y, c.x + 1),
      Coord(c.y + 1, c.x - 1), Coord(c.y + 1, c.x), Coord(c.y + 1, c.x + 1))
      .filter(cc => cc.y >= 0 && cc.x >= 0 && cc.y < ys.end && cc.x < xs.end)

  def to1d(c: Coord) = ys.end * c.y + c.x

  val cells = ys
    .flatMap(y => xs.map(Coord(y, _))) //coords
    .map(c => Cell(to1d(c), neighbours(c).map(to1d))) //cells

  val energies = arr.flatten

  def tick(): Int = {
    cells.foreach(c => energies(c.i) += 1) //increment everything by 1
    val flashers = Iterator.from(0).map(_ => flash()).takeWhile(_ > 0).sum
    cells.foreach(c => if (energies(c.i) == -1) energies(c.i) = 0) //Set new flashers back to 0
    flashers
  }

  def flash(): Int = {
    cells //Increment by adjacent flashers
      .map(c => (c, c.n.count(n => energies(n) > 9)))
      .foldLeft(0) { case (flashCount, (c, inc)) => energies(c.i) match {
        case x if x > 9 =>
          energies(c.i) = -1 // set flashers to -1
          flashCount + 1
        case x =>
          if (x != -1) energies(c.i) += inc // increment by adjacent flashers
          flashCount
      }
      }
  }

  val ticker = Iterator.from(1).map((_, tick()))
  //1
  println(ticker.take(100).map(_._2).sum)
  //2
  println(ticker.dropWhile(_._2 < 100).next()._1)
}
