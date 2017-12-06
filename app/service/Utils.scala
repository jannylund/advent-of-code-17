package service

object Utils {

  /**
    * Day 1 part 1
    */
  def sumNextIfEqual(input: String) = {
    val n = stringAsIntList(input)
    (n :+ n.head).sliding(2).toList.map(l => if (l(0) == l(1)) l(0) else 0).sum
  }

  /**
    * Day 1 part 2
    */
  def sumIfHalfWayEqual(input: String) = {
    val n = stringAsIntList(input)
    val size = n.size
    val offset = size / 2

    val values = n.zipWithIndex.map {
      case (value, index) =>
        if (value == n((index + offset) % size)) {
          value
        } else {
          0
        }
    }
    values.sum
  }

  def stringAsIntList(str: String): List[Int] = {
    str.toList.map(c => Integer.parseInt(c.toString));
  }


  /**
    * Day 2 part 1
    */
  def calcChecksum(input: String) = {
    input.split("\n").map(row => calcRowDiff(row)).sum
  }

  def calcRowDiff(input: String) = {
    val n = stringSplitToIntList(input)
    n.max - n.min
  }

  /**
    * Day 2 part 2
    */
  def calcChecksum2(input: String) = {
    input.split("\n").map(row => calcRowDivide(row)).sum
  }

  // Find a pair of numbers that can return an even value when divided by each other.
  // There is only one such pair per row, so we can quit when we find it.
  def calcRowDivide(input: String) = {
    val n = stringSplitToIntList(input).filter(v => v != 0)
    val d = n.map(dividend => (dividend, getDivisor(dividend, n))).filter(dd => dd._2 > 0).head
    d._1 / d._2
  }

  /* Valid divisors are always smaller than dividend and larger than zero. */
  def getDivisor(dividend: Int, divisors: List[Int]): Int = {
    divisors
      .filter(d => d < dividend && d > 0)
      .filter(d => dividend % d == 0)
      .headOption
      .getOrElse(0)
  }


  def stringSplitToIntList(str: String): List[Int] = {
    str.split("\\s+").toList.map(c => Integer.parseInt(c));
  }
}

