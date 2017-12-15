package game

import game.Day10._
import org.scalatestplus.play.PlaySpec
import utils.Helpers.time

class Day10Test  extends PlaySpec {
  "Day 10 part 1" must {
    "reverse steps properly" in {
      val len = List(3, 4, 1, 5)
      reverseHash((0 to 4).toArray, len) mustBe Array(3, 4, 2, 1, 0)
      getSimpleHash((0 to 4).toArray, len) mustBe 12
    }
    "validate challenge" in {
      val len = List(206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3)
      getSimpleHash((0 to 255).toArray, len) mustBe 9656
    }
  }

  "Day 10 part 2" must {
    "convert to ascii" in {
      strToAscii("1,2,3") mustBe List(49,44,50,44,51)
      strToAsciiWithSuffix("1,2,3") mustBe List(49,44,50,44,51,17,31,73,47,23)
    }
    "xor a list into a int" in {
      xor(List(65,27,9,1,4,3,40,50,91,7,6,0,2,5,68,22)) mustBe 64
    }
    "list to hex string" in {
      makeHex(List(64,7,255)) mustBe "4007ff"
    }
    "hash some strings" in {
      time("getHash ") { getHash("") } mustBe "a2582a3a0e66e6e86e3812dcb672a272"
      time("getHash ") { getHash("AoC 2017") } mustBe "33efeb34ea91902bb2f59c9920caa6cd"
      time("getHash ") { getHash("1,2,3") } mustBe "3efbe78a8d82f29979031a4aa0b16a9d"
      time("getHash ") { getHash("1,2,4") } mustBe "63960835bcdc130f0b66d7ff4f6a5a8e"
      time("getHash ") { getHash("206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3") } mustBe "20b7b54c92bf73cf3e5631458a715149"
    }
  }
}
