package core

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OutputFormatterSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {

  object FormatterTester extends OutputFormatter {
    def test(str: String): String = normaliseCurrency(str)
  }

  "OutputFormatter.format" should {
    "format 1000 as 1,000" in {
      FormatterTester.test("1000") should === ("1,000")
    }
    "format 10000 as 10,000" in {
      FormatterTester.test("10000") should === ("10,000")
    }
    "format 100000 as 100,000" in {
      FormatterTester.test("100000") should === ("100,000")
    }
    "format 1000000 as 1000,000" in {
      FormatterTester.test("1000000") should === ("1,000,000")
    }
    "format 100000000 as 10,000,000" in {
      FormatterTester.test("10000000") should === ("10,000,000")
    }
    "format 100000000 as 100,000,000" in {
      FormatterTester.test("100000000") should === ("100,000,000")
    }

    "format 100 as 100" in {
      FormatterTester.test("100") should === ("100")
    }
    "format 10 as 10" in {
      FormatterTester.test("10") should === ("10")
    }
    "format 1 as 1" in {
      FormatterTester.test("1") should === ("1")
    }
  }
}
