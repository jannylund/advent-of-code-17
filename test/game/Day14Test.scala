package game

import utils.Helpers._
import game.Day14._
import org.scalatestplus.play.PlaySpec

class Day14Test extends PlaySpec {
  val test = "flqrgnkx"
  val challenge = "vbqugkhl"

  "Day 14 Part 1" must {
    "convert hex to binary" in {
      hexToBinary("0") mustBe "0000"
      hexToBinary("1") mustBe "0001"
      hexToBinary("e") mustBe "1110"
      hexToBinary("f") mustBe "1111"
      hexToBinary("a0c2017") mustBe "1010000011000010000000010111"
    }
    "calculate used bits" in {
      time("calcUsed test") { calcUsed(test) } mustBe 8108
      time("calcUsed challenge") { calcUsed(challenge) } mustBe 8148
    }
  }

  "Day 14 Part 2" must {
    "calculate regions" in {
      time("calcRegions sample") { calcRegions(test) } mustBe 1242
      time("calcRegions challenge") { calcRegions(challenge) } mustBe 1180
    }
  }
}
