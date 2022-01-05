package a2021

object A14 extends App {
  val lines = util.loadInputLines(getClass)
  val template = lines.next
  lines.drop(1)
  val r = "(.)(.) -> (.)".r

  val rules = lines.map { case r(c1, c2, res) => ((c1(0), c2(0)), c1 + res) }.toMap

  def insert(template: String): String = {
    template
      .zip(template.tail)
      .flatMap(rules(_))
      .mkString
      .appended(template.last)
  }

  val i20Counts = rules
    .keys
    .map(_.productIterator.mkString)
    .map(k => (k, charCounts(sim20(k).drop(1)))) //Need to deduct the first letter of each one because of overlaps
    .toMap

  val i20 = sim20(template)

  val i40CharCounts = i20
    .zip(i20.tail)
    .map(_.productIterator.mkString)
    .map(i20Counts(_))
    .reduce((mapA, mapB) => (mapA.iterator ++ mapB.iterator)
      .to(LazyList)
      .groupMapReduce(_._1)(_._2)(_ + _)
    )
    .toSeq
    .sortBy(_._2)

  val minChar = i40CharCounts.head
  val maxChar = i40CharCounts.last

  val min = minChar._2 + (if (minChar._1 == i20.head) 1 else 0)
  val max = maxChar._2 + (if (maxChar._1 == i20.head) 1 else 0)

  def sim20(template: String) = {
    (0 to 19).foldLeft(template) { (template, x) => insert(template) }
  }

  def charCounts(template: String) = {
    template
      .toCharArray
      .groupMapReduce(identity)(_ => 1L)(_ + _)
  }
}
