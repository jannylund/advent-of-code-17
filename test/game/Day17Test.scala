package game

import game.Day17._
import org.scalatestplus.play.PlaySpec
import utils.Helpers.time

class Day17Test extends PlaySpec {
  "Day 17 Part 1" must {
    "generate spinlock" in {
      spin(3) mustBe 638
      time("spin 301") { spin(301) } mustBe 1642
    }
  }
  "Day 17 Part 2" must {
    "find value after zero" in {
      time("spin fast 301") { spinFast(301) } mustBe 33601318
    }
  }
}
