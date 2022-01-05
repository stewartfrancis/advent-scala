package a2021

object A3 extends App {
  def bits = util.loadInputLines(getClass)
    .toList
    .map(s => s.chars().toArray.toList.map(_.toChar))

  def gamma = bits
    .flatMap(_.zipWithIndex)
    .filter(_._1 == '1')
    .groupMap(_._2)(_._1)
    .filter(_._2.size > 500)
    .keys
    .map(11 - _)
    .map(1 << _)
    .reduce(_ | _)

  println(gamma * (4095 - gamma)) //Invert the bits!

  def bitCriteria(targetBitProducer: List[Char] => Char) =
    Range(0, bits.head.size, 1)
      .foldLeft(bits) { (bits, i) => {
        if (bits.size == 1) {
          bits
        } else {
          bits.filter(b => targetBitProducer(bits.map(_ (i))) == b(i))
        }
      }
      }.head.mkString

  val oxygenBits = bitCriteria { bits =>
    def threshold = bits.size - (bits.size / 2)

    def ones = bits.count(_ == '1')

    if (ones >= threshold) '1' else '0'
  }

  val co2Bits = bitCriteria { bits =>
    def threshold = bits.size / 2

    def zeros = bits.count(_ == '0')

    if (zeros <= threshold) '0' else '1'
  }

  println(Integer.parseInt(oxygenBits, 2) * Integer.parseInt(co2Bits, 2))
}
