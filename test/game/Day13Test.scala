package game

import game.Day13._
import utils.Helpers._
import org.scalatestplus.play.PlaySpec

class Day13Test extends PlaySpec {

  val test = "0: 3\n1: 2\n4: 4\n6: 4"
  val challenge = "0: 3\n1: 2\n2: 6\n4: 4\n6: 4\n8: 8\n10: 9\n12: 8\n14: 5\n16: 6\n18: 8\n20: 6\n22: 12\n24: 6\n26: 12\n28: 8\n30: 8\n32: 10\n34: 12\n36: 12\n38: 8\n40: 12\n42: 12\n44: 14\n46: 12\n48: 14\n50: 12\n52: 12\n54: 12\n56: 10\n58: 14\n60: 14\n62: 14\n64: 14\n66: 17\n68: 14\n72: 14\n76: 14\n80: 14\n82: 14\n88: 18\n92: 14\n98: 18"

  "Day 13 Part 1" must {
    "know the severity of moving through memory" in {
      time("calculateSeverity test") { calculateSeverity(test) } mustBe 24
      time("calculateSeverity challenge") { calculateSeverity(challenge) } mustBe 632
    }
  }

  "Day 13 Part 2" must {
    "find the delay that allows us to pass through without risk" in {
      time("findDelay sample") { findDelay(test) } mustBe 10
      time("findDelay challenge") { findDelay(challenge) } mustBe 3849742
    }
  }
}
