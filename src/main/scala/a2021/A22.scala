package a2021

import scala.annotation.tailrec

object A22 extends App {
  val lines = util.loadInputLines(getClass)

  case class Cuboid(xs: Range, ys: Range, zs: Range) {
    def vol: Long = xs.size.toLong * ys.size.toLong * zs.size.toLong

    def subtract(c: Cuboid): List[Cuboid] = List(
      Cuboid(xs.start until c.xs.start, ys, zs),
      Cuboid(c.xs.start to c.xs.last, ys.start to c.ys.last, zs.start until c.zs.start),
      Cuboid(c.xs.start to c.xs.last, c.ys.last + 1 to ys.last, zs.start to c.zs.last),
      Cuboid(c.xs.start to c.xs.last, c.ys.start to ys.last, c.zs.end + 1 to zs.last),
      Cuboid(c.xs.start to c.xs.last, ys.start until c.ys.start, c.zs.start to zs.last),
      Cuboid(c.xs.end + 1 to xs.last, ys, zs)
    )
      .filter(_.vol > 0)

    def intersect(other: Cuboid): Option[Cuboid] = {
      intersectRanges(xs, other.xs)
        .flatMap(xi => intersectRanges(ys, other.ys)
          .flatMap(yi => intersectRanges(zs, other.zs)
            .map(zi => Cuboid(xi, yi, zi)))
        )
    }
  }

  case class Instruction(turnOn: Boolean, cuboid: Cuboid)

  val r = "(.*?) x=(.*?)\\.\\.(.*?),y=(.*?)\\.\\.(.*?),z=(.*?)\\.\\.(.*?)".r

  val instructions = lines
    .map { case r(onoff, xf, xt, yf, yt, zf, zt) => Instruction(
      "on" == onoff,
      Cuboid(xf.toInt to xt.toInt, yf.toInt to yt.toInt, zf.toInt to zt.toInt)
    )
    }
    .toList

  def intersectRanges(r1: Range, r2: Range) = {
    Option(Math.max(r2.start, r1.start) to Math.min(r2.last, r1.last))
      .filter(_ => (r1.start <= r2.last && r1.last >= r2.start) || (r2.start <= r1.last && r2.last >= r1.start))
  }

  @tailrec
  def applyInstructions(states: List[Cuboid], instructions: List[Instruction]): List[Cuboid] = instructions match {
    case ::(head, next) => applyInstructions(applyInstruction(states, head), next)
    case Nil => states
  }

  def applyInstruction(states: List[Cuboid], instruction: Instruction): List[Cuboid] = {
    val newStates = states
      .flatMap(state => state
        .intersect(instruction.cuboid)
        .map(state.subtract)
        .getOrElse(List(state))
      )

    if (instruction.turnOn) {
      instruction.cuboid :: newStates
    } else {
      newStates
    }
  }

  //1
  println(applyInstructions(Nil, instructions.take(20)).map(_.vol).sum)

  //2
  println(applyInstructions(Nil, instructions).map(_.vol).sum)
}
