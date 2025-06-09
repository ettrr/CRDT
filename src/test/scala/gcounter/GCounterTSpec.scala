package gcounter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class GCounterTSpec extends AnyFlatSpec with Matchers {
  private val counterT = GCounterT[Map, String, Int]

  "merge" should "combine maps correctly" in {
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)
    val merge = counterT.merge(g1, g2)
    merge shouldBe Map("a" -> 7, "b" -> 5)
    counterT.total(merge) shouldBe 12
  }


  "merge" should "be commutative operation" in {
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)
    counterT.merge(g1, g2) shouldBe counterT.merge(g2, g1)
  }

  "merge" should "be idempotent operation" in {
    val g0 = Map("a" -> 7, "b" -> 4)
    val g1 = Map("a" -> 8, "b" -> 1)
    counterT.merge(counterT.merge(counterT.merge(g0, g1), g1), g1) shouldBe counterT.merge(g0, g1)
  }

  "increment" should "update value on input const" in {
    val g = Map("a" -> 7, "b" -> 3)
    counterT.increment(g)("a", 3) shouldBe Map("a" -> 10, "b" -> 3)
  }

  it should "correct work with multiple operations" in {
    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 1, "b" -> 0, "c" -> 7)
    val g3 = Map("a" -> 2, "b" -> 4, "d" -> 11)
    val g4 = Map("a" -> 4, "b" -> 5, "e" -> 2)
    val g5 = Map("a" -> 10, "b" -> 6, "f" -> 9)

    counterT.merge(g1, g4) shouldBe Map("a" -> 7, "b" -> 5, "e" -> 2)
    counterT.merge(g3, g5) shouldBe Map("a" -> 10, "b" -> 6, "d" -> 11, "f" -> 9)

    val g1Update = counterT.increment(g1)("n", 17)
    val g6 = counterT.increment(Map.empty)("s", 11)

    counterT.merge(g1Update, g2) shouldBe Map("a" -> 7, "b" -> 3, "n" -> 17, "c" -> 7)
    counterT.merge(g6, g1) shouldBe Map("s" -> 11, "a" -> 7, "b" -> 3)

    counterT.merge(g1, g1) shouldBe g1
  }

}
