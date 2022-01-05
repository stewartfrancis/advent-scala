package a2021

object A19 extends App {

  val lines = util.loadInputLines(getClass)

  case class Scanner(bs: Seq[Vector]) {
    def rotateX(): Scanner = Scanner(bs.map(_.rotateX()))

    def rotateY(): Scanner = Scanner(bs.map(_.rotateY()))

    def rotateZ(): Scanner = Scanner(bs.map(_.rotateZ()))
  }

  case class Vector(x: Int, y: Int, z: Int) {
    def -(other: Vector): Vector = Vector(x - other.x, y - other.y, z - other.z)

    def +(other: Vector): Vector = Vector(x + other.x, y + other.y, z + other.z)

    def rotateX(): Vector = Vector(x, -z, y)

    def rotateY(): Vector = Vector(z, y, -x)

    def rotateZ(): Vector = Vector(-y, x, z)
  }

  def orientations(s: Scanner): List[Scanner] = {
    val x1 = s //x
    val z1 = x1.rotateY() //z
    val y1 = x1.rotateX().rotateY() //y
    val mx1 = z1.rotateY() //-x
    val my1 = x1.rotateZ() //-y
    val mz1 = mx1.rotateY() //-z
    List(x1, mx1, y1, my1, z1, mz1).flatMap(s => (0 to 2).foldLeft(List(s))((acc, _) => acc.head.rotateX() :: acc)) //All rotations
  }

  def compare(s1: Scanner, s2: Scanner): Option[(Scanner, Vector)] = {
    orientations(s2)
      .iterator
      .map { s2 =>
        s1.bs //Only have to consider 1 orientation from s1, and all from s2
          .flatMap(b1 => s2.bs.map(b2 => b1 - b2)) //Map each point in S1 to a list of vectors to each point in S2
          .groupBy(identity) //Group common vectors
          .find(_._2.length >= 12) //If there are 12+ common vectors, that vector describes the offset between the scanners
          .map(v => (s2, v._1))
      }
      .find(o => o.isDefined)
      .flatten
  }

  def findScanners(checked: Set[(Scanner, Vector)], u: List[Scanner], sv: (Scanner, Vector)): (Set[(Scanner, Vector)], List[Scanner]) = {
    if (!checked.contains(sv)) {
      val (unchecked, unfound) = u.map(s2 => (s2, compare(sv._1, s2))).partitionMap { case (s, o) => o match {
        case Some((foundScanner, vFromS)) => Left((foundScanner, sv._2 + vFromS))
        case None => Right(s)
      }
      }

      unchecked.foldLeft((checked + sv, unfound)) { case ((checked, unfound), usv) =>
        findScanners(checked, unfound, usv)
      }
    } else {
      (checked, u)
    }
  }

  val r = "\\s*(.*?),(.*?),(.*)".r

  val scanners = lines
    .foldRight(List(List[Vector]())) { (line, acc) =>
      line match {
        case x if x.startsWith("---") => List() :: acc
        case r(x, y, z) => (Vector(x.toInt, y.toInt, z.toInt) :: acc.head) :: acc.tail
        case _ => acc
      }
    }
    .tail
    .map(Scanner)

  //1
  val relScanners = findScanners(Set(), scanners.tail, (scanners.head, Vector(0, 0, 0)))._1
  println(relScanners.map(_._2))
  println(relScanners.flatMap(s => s._1.bs.map(b => b + s._2)).size)

  //2
  println(
    relScanners
      .toList
      .combinations(2)
      .map { case s1 :: s2 :: Nil => s1._2 - s2._2 } //Convert to vector between pair of scanners
      .map(v => Math.abs(v.x) + Math.abs(v.y) + Math.abs(v.z))
      .max
  )
}
