package service

import org.scalatestplus.play.PlaySpec
import Utils._

class UtilsTest extends PlaySpec {
  "Day 1" must {
    "summarize values in string if next is equal" in {
      sumNextIfEqual("1122") mustBe 3
      sumNextIfEqual("1111") mustBe 4
      sumNextIfEqual("1234") mustBe 0
      sumNextIfEqual("91212129") mustBe 9
    }
  }
}
