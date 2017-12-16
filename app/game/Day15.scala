package game

import scala.annotation.tailrec

object Day15 {
  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  def generate(start: Int, factor: Int): Iterator[BigInt] = {
    val x = new Generator(start, factor)
    x.values
  }

  def bin(in: Iterator[BigInt]): Iterator[String] = {
    in.map(v => v.toInt.toBinaryString.takeRight(16))
  }

  def moduloFilter(in: Iterator[BigInt], mod: Int): Iterator[BigInt] = {
    in.filter(i => i % mod == 0)
  }

  @tailrec
  def compare(genA: Iterator[String], genB: Iterator[String], len: Int = 0, acc: Int = 0): Int = {
    if (len < 1) {
      acc
    } else {
      compare(genA, genB, len - 1, acc + (genA.next == genB.next))
    }
  }
}

class Generator(start: Int, factor: Int) {
  val divisor = 2147483647
  val values: Iterator[BigInt] = Iterator.iterate(BigInt(start * factor)) { case (x) => (x * factor % divisor) }
}
