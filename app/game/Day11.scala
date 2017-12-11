package game

object Day11 {
  val start = Pos(0, 0, 0)

  def distance(str: String) = {
    travel(start, str.split(","))._1
  }

  def maxDistance(str: String) = {
    travel(start, str.split(","))._2
  }

  def travel(p: Pos = Pos(0, 0, 0), moves: Seq[String], max: Int = 0): (Int, Int) = {
    if (moves.isEmpty) {
      (measure(p), max)
    } else {
      travel(move(moves.head, p), moves.tail, Math.max(max, measure(p)))
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
