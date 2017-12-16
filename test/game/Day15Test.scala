package game

import game.Day15._
import utils.Helpers._
import org.scalatestplus.play.PlaySpec

class Day15Test extends PlaySpec {
  def genA: Iterator[BigInt] = generate(65, 16807)
  def genB: Iterator[BigInt] = generate(8921, 48271)

  def cGenA: Iterator[BigInt] = generate(634, 16807)
  def cGenB: Iterator[BigInt] = generate(301, 48271)

  "Day 15 Part 1" must {
    "generate with a given number of values" in {
      genA.take(5).toList mustBe List(1092455, 1181022009, 245556042, 1744312007, 1352636452)
      genB.take(5).toList mustBe List(430625591, 1233683848, 1431495498, 137874439, 285222916)
    }
    "compare generators and find matches in lowest 16 bits of binary rep" in {
      compare(bin(genA), bin(genB), 5) mustBe 1
      time("compare 40M (sample)") { compare(bin(genA), bin(genB), 40000000) } mustBe 588
      time("compare 40M (challenge)") { compare(bin(cGenA), bin(cGenB), 40000000) } mustBe 573
    }
  }

  "Day 15 Part 2" must {
    "generate with a given number of values and a modulo" in {
      moduloFilter(genA, 4).take(5).toList mustBe List(1352636452, 1992081072, 530830436, 1980017072, 740335192)
      moduloFilter(genB, 8).take(5).toList mustBe List(1233683848, 862516352, 1159784568, 1616057672, 412269392)
    }
    "compare generators and find matches in lowest 16 bits of binary rep" in {
      time("compare mod 5M (sample)") { compare(bin(moduloFilter(genA, 4)), bin(moduloFilter(genB, 8)), 5000000) } mustBe 309
      time("compare mod 5M (challenge)") { compare(bin(moduloFilter(cGenA, 4)), bin(moduloFilter(cGenB, 8)), 5000000) } mustBe 294
    }
  }
}
