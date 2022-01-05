package a2021

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object A16 extends App {
  val input = util.loadInputLines(getClass).next()

  val bits = input
    .toCharArray
    .flatMap { x =>
      "%4s"
        .format(Integer.toBinaryString(Integer.parseInt("" + x, 16)))
        .replace(' ', '0')
        .toCharArray
    }
    .toList

  case class Packet(v: Long, subpackets: List[Packet], op: List[Packet] => Long) {
    def getVal: Long = op.apply(subpackets)
  }

  val p = readPacket(bits)

  println(sumVersions(p._1))
  println(p._1.getVal)

  def sumVersions(p: Packet): Long = p.v + p.subpackets.map(sumVersions).sum

  def readNumber(bits: List[Char], width: Int): (Long, List[Char]) = {
    val (numBits, afterNum) = bits.splitAt(width)
    (java.lang.Long.parseLong(numBits.mkString, 2), afterNum)
  }

  def readLiteral(version: Long, bits: List[Char]): (Packet, List[Char]) = {
    def readLiteralR(bits: List[Char]): (List[Char], List[Char]) = {
      bits match {
        case '1' :: a :: b :: c :: d :: rest =>
          val (suffix, remaining) = readLiteralR(rest)
          (a :: b :: c :: d :: suffix, remaining)
        case '0' :: a :: b :: c :: d :: rest => (a :: b :: c :: d :: Nil, rest)
      }
    }

    val (literalBits, rest) = readLiteralR(bits)
    val literal = readNumber(literalBits, literalBits.size)._1
    (Packet(version, List.empty, x => literal), rest) //literal
  }

  def readHeader(bits: List[Char]): (Long, Long, List[Char]) = {
    val (version, afterVersion) = readNumber(bits, 3)
    val (typpe, afterType) = readNumber(afterVersion, 3)
    (version, typpe, afterType)
  }

  def readOperator(version: Long, typpe: Long, bits: List[Char]): (Packet, List[Char]) = {
    val (subPackets, rest) = bits match { //operator
      case '0' :: rest => readType0Subpackets(rest)
      case '1' :: rest => readType1Subpackets(rest)
    }

    val op: List[Packet] => Long = typpe match {
      case 0 => ps => ps.map(_.getVal).sum
      case 1 => ps => ps.map(_.getVal).product
      case 2 => ps => ps.map(_.getVal).min
      case 3 => ps => ps.map(_.getVal).max
      case 5 => ps => if (ps.head.getVal > ps.tail.head.getVal) 1 else 0
      case 6 => ps => if (ps.head.getVal < ps.tail.head.getVal) 1 else 0
      case 7 => ps => if (ps.head.getVal == ps.tail.head.getVal) 1 else 0
    }
    (Packet(version, subPackets, op), rest)
  }

  def readType0Subpackets(bits: List[Char]): (List[Packet], List[Char]) = {
    @tailrec
    def readPacketsExhaustively(packets: ListBuffer[Packet], bits: List[Char]): ListBuffer[Packet] = {
      bits match {
        case Nil => packets
        case x =>
          val (packet, rest) = readPacket(x)
          packets.append(packet)
          readPacketsExhaustively(packets, rest)
      }
    }

    val (subpacketsWidth, afterWidth) = readNumber(bits, 15)
    val (subpacketsBits, afterSubpackets) = afterWidth.splitAt(subpacketsWidth.toInt)
    (readPacketsExhaustively(ListBuffer[Packet](), subpacketsBits).toList, afterSubpackets)
  }

  def readType1Subpackets(bits: List[Char]): (List[Packet], List[Char]) = {
    val (numSubpackets, afterNumSubpackets) = readNumber(bits, 11)
    val subpackets = (0 until numSubpackets.toInt)
      .foldLeft((ListBuffer[Packet](), afterNumSubpackets)) { case ((packets, bits), _) =>
        val (packet, afterPacket) = readPacket(bits)
        (packets.append(packet), afterPacket)
      }
    (subpackets._1.toList, subpackets._2)
  }

  def readPacket(bits: List[Char]): (Packet, List[Char]) = {
    val (version, typpe, afterHeader) = readHeader(bits)
    if (typpe == 4) {
      readLiteral(version, afterHeader)
    } else {
      readOperator(version, typpe, afterHeader)
    }
  }
}
