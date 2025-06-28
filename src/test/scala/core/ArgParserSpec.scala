package core

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import core.RetCalcError.{InvalidArgFormatError, InvalidNumberError}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArgParserSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {

  "ArgParser.parseInt" should {
    "return a valid integer value, after being parsed from a string" in {
      ArgParser.parseInt("VALID-VALUE", "25") should === (Valid(25))
    }
    "return an InvalidNumberError within a NonEmptyList when invalid string argument provided" in {
      val argName = "INVALID-VALUE"
      val argValue = "25.0"
      ArgParser.parseInt(argName, argValue) should === (
        Invalid(NonEmptyList.of(InvalidNumberError(argName, argValue)))
      )
    }
  }

  "ArgParser.parseDouble" should {
    "return a valid double value, after being parsed from a string" in {
      ArgParser.parseDouble("VALID-VALUE", "135.02") should === (Valid(135.02))
    }
    "return an InvalidNumberError within a NonEmptyList when invalid string argument provided" in {
      val argName = "INVALID-VALUE"
      val argValue = "24£000.02"

      ArgParser.parseDouble(argName, argValue) should === (
        Invalid(NonEmptyList.of(InvalidNumberError(argName, argValue)))
      )
    }
  }

  "ArgParser.parseParams" should {
    "return a valid RetCalcParams object, after parsing array of string args" in {
      ArgParser.parseParams(
        Array(
          "1957.09:2017.09",
          "25",
          "30",
          "3000",
          "2000",
          "10000"
        )
      ) should === (
        Valid(
          RetCalcParams(
            initialCapital = 10000,
            nMonthsInRetirement = 30 * 12,
            netIncome = 3000,
            expenses = 2000
          )
        )
      )
    }
    "return a collection of InvalidNumberError objects within " +
      "a NonEmptyList when invalid string arguments provided" in {
      val invalidInitialCapitalArg = "initialCapital"
      val invalidInitialCapital = "3'000"

      val invalidNYearsRetirementArg = "nbYearsInRetirement"
      val invalidNYearsRetirement = "invalid string"

      val invalidExpensesArg = "expenses"
      val invalidExpenses = "2,000"

      val validIncome = "3000"
      val expected = NonEmptyList.of(
        InvalidNumberError(invalidNYearsRetirementArg, invalidNYearsRetirement),
        InvalidNumberError(invalidExpensesArg, invalidExpenses),
        InvalidNumberError(invalidInitialCapitalArg, invalidInitialCapital),
      )

      ArgParser.parseParams(
        Array(
          "1957.09:2017.09",
          "25",
          invalidNYearsRetirement,
          validIncome,
          invalidExpenses,
          invalidInitialCapital
        )
      ) should === (Invalid(expected))
    }
  }

  "ArgParser.parseFromUntil" should {
    "return a validated tuple value containing strings 'from' and 'until', " +
      "after being parsed from a string" in {
      ArgParser.parseFromUntil("1957.09:2017.09") should === (Valid(("1957.09", "2017.09")))
    }
    "return an InvalidArgFormatError within a NonEmptyList when invalid string argument provided" in {

      val invalidFromUntilArg = "FROM,UNTIL"
      val invalidFromUntil = "1957.09,2017.09"

      val expectedFromUntilArg = "FROM:UNTIL"

      val expected = NonEmptyList.of(InvalidArgFormatError("fromUntil", invalidFromUntil, expectedFromUntilArg))

      ArgParser.parseFromUntil(invalidFromUntil) should === (
        Invalid(expected)
      )
    }
  }
}
