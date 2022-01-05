package a2021

object A17 extends App {
  val xs = 60 to 94
  val ys = -171 to -136

  //1
  println(sumTri(ys.start))

  //2
  val yDistances = (ys.start until -ys.start)
    .map(yv => if (yv > 0) (yv, (2 * yv) + 1, -(yv + 1)) else (yv, 0, yv)) //Adjust offset for positive values so we can do the same maths
    .flatMap(yvtv => { //Now transform to eligible points by...
      val deduct = sumTri(-(yvtv._3 + 1))
      Iterator
        .from(-yvtv._3)
        .map(y => -(sumTri(y) - deduct)) //Calculate successive offsets from 0
        .takeWhile(_ >= ys.start) //Limit to before the end of the target range
        .zipWithIndex //Zip with number of index for turns
        .filter(_._1 <= ys.end) //Filter by after the start of the target range
        .map { case (y, i) => (yvtv._1, y, i + yvtv._2 + 1) } //Add initial v and the base number of turns from the start
        .toList
    })
    .toList
    .groupMap(_._3)(point => (point._1, point._2)) //Turns -> (yv, distance)

  val xDistances = Iterator
    .from(0)
    .takeWhile(_ <= xs.end)
    .flatMap { xv => {
      val rootTri = sumTri(xv)
      Iterator
        .from(0)
        .takeWhile(_ <= xv)
        .map(nx => rootTri - sumTri(xv - nx))
        .zipWithIndex
        .filter(point => xs.contains(point._1))
        .map(point => (xv, point._1, point._2))
        .toList
    }
    }
    .toList
    .groupMap(_._3)(point => (point._1, point._2)) //Turns -> (xv, distance)

  //First pair all X with all Ys where they have the same number of turns
  val validVelocities1 = xDistances
    .iterator
    .flatMap { case (numTurns, xPoints) =>
      yDistances
        .get(numTurns)
        .map(yPoints => yPoints.flatMap(yPoint => xPoints.map(xPoint => (xPoint._1, yPoint._1))))
        .getOrElse(List())
    }
    .toSet

  //Additionally because when X has stopped moving, y can take an arbitrary number more turns, we need to find those pairs too
  val validVelocities2 = xDistances
    .iterator
    .flatMap { case (numTurns, points) => points.filter(_._1 == numTurns) }
    .flatMap { xPoint =>
      yDistances
        .iterator
        .filter { case (numTurns, _) => numTurns > xPoint._1 }
        .flatMap { case (_, yPoints) => yPoints.map(yPoint => (xPoint._1, yPoint._1)) }
    }
    .toSet

  println((validVelocities1 ++ validVelocities2).size)

  def sumTri(n: Int) = (n * (n + 1)) / 2
}
