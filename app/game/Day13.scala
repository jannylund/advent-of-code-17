package game

object Day13 {
  def calculateSeverity(str: String) = {
    parse(str)
      .filter(l => isRisky(l))
      .map(l => l.depth * l.range)
      .sum
  }

  def findDelay(str: String) = {
    val p = parse(str)
    var d = 0
    do {
      d = d + 1
    } while (!p.filter(l => isRisky(l, d)).isEmpty)
    d
  }

  def isRisky(l: Layer, o: Int = 0): Boolean = {
    (l.depth + o) == 0 || (l.depth + o) % (2 * l.range - 2) == 0
  }

  def parse(str: String): Seq[Layer] = {
    val lines = str.replace(" ", "").split("\n")
    lines
      .map(l => l.split(":"))
      .map({ case Array(key, value) => Layer(key.toInt, value.toInt) })
  }

  case class Layer(depth: Int, range: Int)

}
