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

  /**
    * Day 3
    *
    * Position is a spiral around the point (0,0). Get the proper tuple for any integer > 0
    */

  def getDistance(pos: Pos): Int = {
    pos.x.abs + pos.y.abs
  }

  def getFirstLargerThan(value: Int): Int = {
    var x = 0
    var sum = 0

    while(sum <= value) {
      x = x + 1
      sum = getSumofAdjacent(x);
    }
    sum
  }

  def getSumofAdjacent(i: Int): Int = {
    // 1 is a special case.
    if (i == 1) {
      1
    } else {
      // recursive from 1 to n to calculate value for all steps.
      // we start with 2 since 1 is a special case, and we need to add that to acc.
      recCalcValue(i, 2, 1, Map(Pos(0,0) -> 1))
    }
  }

  // recursive calculate value from adjacent neigbors
  def recCalcValue(max: Int, curr: Int, value: Int, acc: Map[Pos, Int]): Int = {
    if(curr > max) {
      value
    } else {
      val sum = getNeighbors(curr).map(p => acc.getOrElse(p, 0)).sum
      recCalcValue(max, curr + 1, sum, acc + (getPosition(curr) -> sum))
    }
  }

  def getNeighbors(i: Int): List[Pos] = {
    if (i < 2) {
      List.empty
    } else {
      val curr = getPosition(i)
      (1 to (i - 1)).toList.map(i => getPosition(i)).filter(p => areNeighbors(p, curr))
    }
  }

  def areNeighbors(p1: Pos, p2: Pos): Boolean = {
    (p1.y - p2.y).abs <= 1 && (p1.x - p2.x).abs <= 1
  }

  def getPosition(i: Int): Pos = {
    walkPos(i, Pos(0, 0), Down(), 0, 0)
  }

  // stepsLeft == steps left to take before we reach our position
  // currPos == current position
  // s == steps we have taken in current direction
  // m == max steps to take before changing direction
  def walkPos(stepsLeft: Int, currPos: Pos, d: Direction, s: Int, m: Int): Pos = {
    // since our starting point is 1, we also end at 1.
    if (stepsLeft == 1) {
      currPos
    } else {
      // if we have taken all the steps needed this direction, move on.
      if (s >= m) {
        val nextDir = next(d)
        val max = nextDir match {
          case Left() => m + 1
          case Right() => m + 1
          case _ => m
        }
        walkPos(stepsLeft - 1, move(currPos, nextDir), nextDir, 1, max)
      } else {
        walkPos(stepsLeft - 1, move(currPos, d), d, s + 1, m)
      }
    }
  }

  // next direction.
  def next(dir: Direction): Direction = {
    dir match {
      case Right() => Up()
      case Up() => Left()
      case Left() => Down()
      case Down() => Right()
    }
  }

  // move position in the given direction.
  def move(pos: Pos, dir: Direction): Pos = {
    dir match {
      case Right() => Pos(pos.x + 1, pos.y)
      case Up() => Pos(pos.x, pos.y + 1)
      case Left() => Pos(pos.x - 1, pos.y)
      case Down() => Pos(pos.x, pos.y - 1)
    }
  }

  abstract class Direction()

  case class Left() extends Direction

  case class Right() extends Direction

  case class Up() extends Direction

  case class Down() extends Direction

  case class Pos(x: Int, y: Int)

}

