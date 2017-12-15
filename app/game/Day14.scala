package game

import game.Day10.getHash

object Day14 {

  // calc all the number ones.
  def calcUsed(str: String) = {
    grid(str)
      .flatten
      .sum
  }

  // add +1 for the region of unused blocks.
  def calcRegions(str: String) = {
    val used = getUsed(grid(str))
    findRegions(used.toSet, Set(used.head)).size + 1
  }

  // get a list of all coordinates of the positions with ones.
  def getUsed(grid: Array[Array[Int]]) = {
    for (x <- grid.indices; y <- grid(x).indices if grid(x)(y) == 1) yield Pos(x, y)
  }

  def findRegions(used: Set[Pos], group: Set[Pos], regions: Seq[Set[Pos]] = Seq.empty): Seq[Set[Pos]] = {
    if (used.isEmpty) {
      regions
    } else {
      val adjacent = group.flatMap(p => expand(p))
      val matches = used.intersect(adjacent)
      if (!matches.isEmpty) {
        findRegions(used.filterNot(matches), (group ++ matches), regions)
      } else {
        findRegions(used, Set(used.head), regions :+ group)
      }
    }
  }

  // Expand pos to the 4 positions around it.
  def expand(p: Pos): Set[Pos] = {
    val pos = Seq(p, Pos(p.x - 1, p.y), Pos(p.x + 1, p.y), Pos(p.x, p.y - 1), Pos(p.x, p.y + 1))
    pos.filter(p => p.x >= 0 && p.y >= 0).toSet
  }

  def grid(str: String): Array[Array[Int]] = {
    val lines = (0 to 127).map(i => str + "-" + i.toString)
    lines
      .map(l => getHash(l))
      .map(l => hexToBinary(l))
      .map(l => l.toArray.map(_.toString.toInt))
      .toArray
  }

  def hexToBinary(str: String) = {
    str.map(c => "000" + Integer.parseInt(c.toString, 16).toBinaryString takeRight (4)).mkString
  }

  case class Pos(x: Int, y: Int)

}