package bot.test

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TestProva extends AnyFunSuite with Matchers:
  test("Simple test example") {
    val risultato = 42
    risultato shouldBe 42
  }


