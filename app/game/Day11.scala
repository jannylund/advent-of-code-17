package game

object Day11 {
  val start = List(Pos(0, 0, 0))

  def distance(str: String) = {
    measure(travel(start, str.split(",")).head)
  }

  def maxDistance(str: String) = {
    travel(start, str.split(",")).map(p => measure(p)).max
  }

  // Travel gives us a reverse list of positions we visited. So head is always current.
  def travel(hist: List[Pos], moves: Seq[String]): List[Pos] = {
    if (moves.isEmpty) {
      hist
    } else {
      travel(List(move(moves.head, hist.head)) ++ hist, moves.tail)
    }
  }

  def measure(pos: Pos): Int = {
    (Math.abs(pos.x) + Math.abs(pos.y) + Math.abs(pos.z)) / 2
  }

  def move(m: String, p: Pos): Pos = {
    m match {
      case "ne" => Pos(p.x + 1, p.y, p.z - 1)
      case "se" => Pos(p.x + 1, p.y - 1, p.z)
      case "s" => Pos(p.x, p.y - 1, p.z + 1)
      case "sw" => Pos(p.x - 1, p.y, p.z + 1)
      case "nw" => Pos(p.x - 1, p.y + 1, p.z)
      case "n" => Pos(p.x, p.y + 1, p.z - 1)
    }
  }

  case class Pos(x: Int, y: Int, z: Int)

}
