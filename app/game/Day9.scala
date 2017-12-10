package game

object Day9 {
  def getScore(str: String) = {
    calculate(cleanUp(str))
  }

  // find each pair, calculate it's value and drop it out until we are done.
  def calculate(str: String, acc: Int = 0): Int = {
    // find first occurence of {} and start with that.
    val pos = str.indexOfSlice("{}")
    if (pos.equals(-1)) {
      acc
    } else {
      // content before. we actually only need to use that to find the value.
      val before = str.substring(0, pos)
      val after = str.substring(pos + 2, str.size)
      val ups = before.count(_ == '{')
      val downs = before.count(_ == '}')
      calculate(before + after, acc + (ups - downs + 1))
    }
  }

  def cleanUp(str: String) = {
    // first drop any occurrence of ! and the first char after that.
    // then drop any garbage.
    str.replaceAll("!.", "").replaceAll("<.*?>", "").replace(",", "").replace(" ", "")
  }

  def countChar(str: String): Int = {
    val re = "<.*?>".r

    re.findAllIn(str.replaceAll("!.", ""))
      .map(s => s.drop(1).dropRight(1).length)
      .sum
  }
}
