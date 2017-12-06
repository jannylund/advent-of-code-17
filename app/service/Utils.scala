package service

object Utils {

  /**
    * Day 1 part 1
    */
  def sumNextIfEqual(input: String) = {
    val n = stringAsIntList(input)
    (n :+ n.head).sliding(2).toList.map(l => if (l(0) == l(1)) l(0) else 0).sum
  }

  def stringAsIntList(str: String): List[Int] = {
    str.toList.map(c => Integer.parseInt(c.toString));
  }
}
