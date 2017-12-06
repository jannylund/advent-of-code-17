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
        if (value == n((index + offset) % size)){
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
}
