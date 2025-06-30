package app

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.data.Validated.{Valid, Invalid}

class SimulatePlanAppIT extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {

  "SimulatePlanApp.strMain" should {
    "return the expected string result containing the correct capital after retirement, and capital after death" in {

      val expected =
        s"""
          |Capital after 25 years of saving: £468,925.
          |Capital after 40 years in retirement: £2,958,842.
          |""".stripMargin

      SimulatePlanApp.strMain(Array("1952.09:2017.09", "25", "40", "3000", "2000", "10000")) should === (expected)
    }

    "return error message when amount of months saving/in retirement exceed the amount of returns data available" in {
      val expected = "Cannot get returns for month 780. Accepted range: 0 to 779."

      SimulatePlanApp.strMain(Array("1952.09:2017.09", "25", "41", "3000", "2000", "10000")) should === (expected)
    }

    "return a usage statement if the expected number of arguments is not provided" in {

      val expected =
        """
           |Not all arguments provided. Expected 6 arguments. Found 5.
           |
           |Usage: FROM:UNTIL YEARS-SAVING YEARS-IN-RETIREMENT INITIAL-CAPITAL NET-INCOME EXPENSES
           |Retirement calculator. Calculates the amount of capital saved before retirement, and amount of capital left for your loved ones.
           |Args:
           |  FROM:UNTIL: A time window of returns data, provided by sp500 and consumer price index (inflation). Supported dates are between 1900.01 and 2017.09.
           |  YEARS-SAVING: The amount of years you plan to save money.
           |  YEARS-IN-RETIREMENT: The amount of years you plan to live in retirement.
           |  INITIAL-CAPITAL: The amount of money you already have saved.
           |  NET-INCOME: Your monthly net income (after taxes).
           |  EXPENSES: Your monthly expenses.
           |""".stripMargin
      SimulatePlanApp.strMain(Array("1952.09:2017.09", "10", "30", "3000", "2000")) should === (expected)
    }

    "return a collection of error messages in accordance with invalid arguments" in {
      val expected =
        """Invalid format for argument 'fromUntil'. Expected 'FROM:UNTIL'. Found '1957.09,2017.09'.
          |Unable to parse number argument 'nbYearsSaving' with value provided: '25.0'.
          |Unable to parse number argument 'expenses' with value provided: '2'000'.""".stripMargin

      SimulatePlanApp.strMain(Array("1957.09,2017.09", "25.0", "30", "3000", "2'000", "10000")) should === (expected)
    }

    "return an error message returned from MonthIDNotFoundError when non-existent 'until' arg supplied" in {
      val expected = "Unable to locate returns data for monthId '2018.01'." +
        " Minimum month ID: '1900.02'. Maximum monthID: '2017.09'."
      SimulatePlanApp.strMain(Array("1977.01:2018.01", "25", "30", "3000", "2000", "10000")) should === (expected)
    }
  }
}
