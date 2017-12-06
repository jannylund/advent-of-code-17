package game

import game.Day6._
import org.scalatestplus.play.PlaySpec

class Day6Test extends PlaySpec {

  "Day 6 Part 1" must {
    "balance blocks" in {
      balance(Array(0, 2, 7, 0)) mustBe List(2, 4, 1, 2)
      balance(Array(2, 4, 1, 2)) mustBe List(3, 1, 2, 3)
      balance(Array(3, 1, 2, 3)) mustBe List(0, 2, 3, 4)
    }
    "detect infinite loop" in {
      detectInfiniteLoop(List(0, 2, 7, 0)) mustBe 5
      detectInfiniteLoop(List(5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6)) mustBe 5042
    }
  }

  "Day 6 Part 2" must {
    "find repetitions between infinite loops" in {
      detectCountBetweenInfiniteLoops(List(2, 4, 1, 2)) mustBe 4
      detectCountBetweenInfiniteLoops(List(1, 1, 14, 13, 12, 11, 10, 9, 8, 7, 7, 5, 5, 3, 3, 0)) mustBe 1086
    }
  }
}
