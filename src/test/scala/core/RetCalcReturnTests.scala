package core

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import core.RetCalcError.{MonthIDNotFoundError, MonthOutOfBoundsError}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RetCalcReturnTests extends AnyWordSpec with Matchers with TypeCheckedTripleEquals with EitherValues {

  "VariableReturns.fromUntil" should {

    // Used in four unit tests following.
    val variableReturns = VariableReturns(
      Vector.tabulate(12)(x => {
        val d = (x + 1).toDouble
        VariableReturn(
          monthId = f"2017.$d%02.0f",
          monthlyInterest = d
        )
      })
    )
    "keep only the expected window of returns" in {

      val result = variableReturns.fromUntil("2017.01", "2017.08")
      val expected = VariableReturns(
        Vector(
          VariableReturn("2017.01", 1.0),
          VariableReturn("2017.02", 2.0),
          VariableReturn("2017.03", 3.0),
          VariableReturn("2017.04", 4.0),
          VariableReturn("2017.05", 5.0),
          VariableReturn("2017.06", 6.0),
          VariableReturn("2017.07", 7.0)
        )
      )

      result should === (Valid(expected))
    }
    "return MonthIDNotFound error if the specified month provided in 'from' is not found in returns Vector" in {
      val expected = MonthIDNotFoundError("2016.12", "2017.01", "2017.12")

      variableReturns.fromUntil("2016.12", "2017.05") should === (Invalid(NonEmptyList.of(expected)))
    }

    "return MonthIDNotFound error if the specified month provided in 'until' is not found in returns Vector" in {
      val expected = MonthIDNotFoundError("2018.01", "2017.01", "2017.12")
      variableReturns.fromUntil("2017.01", "2018.01") should === (Invalid(NonEmptyList.of(expected)))
    }
    "return MonthIDNotFound error if both 'from' and 'until' month IDs are not found in returns Vector" in pending
  }

  "Returns.monthlyInterest" should {
    "return the expected double values based on input of FixedReturn case class" in {
      val expected = 0.04 / 12
      (1 to 5).foreach {
        x =>
          Returns.monthlyInterest(x, FixedReturn(0.04)).right.value should === (expected)
      }

    }
    "return the three expected double values based on input of VariableReturns" in {
      val returns = VariableReturns(
        Vector(
          VariableReturn("2017.01", 0.1),
          VariableReturn("2017.02", 0.2),
          VariableReturn("2017.03", 0.3)
        )
      )

      Returns.monthlyInterest(0, returns).right.value should === (0.1)
      Returns.monthlyInterest(1, returns).right.value should === (0.2)
      Returns.monthlyInterest(2, returns).right.value should === (0.3)
    }
    "return MonthOutOfBoundsError if the specified month exceeds the length of VariableReturns" in {
      val returns = VariableReturns(
        Vector(
          VariableReturn("2017.01", 0.1),
          VariableReturn("2017.02", 0.2),
          VariableReturn("2017.03", 0.3)
        )
      )

      Returns.monthlyInterest(0, returns).right.value should === (0.1)
      Returns.monthlyInterest(1, returns).right.value should === (0.2)
      Returns.monthlyInterest(2, returns).right.value should === (0.3)
      Returns.monthlyInterest(3, returns).left.value should === (MonthOutOfBoundsError(3, 2))


    }
    "return the expected offset returns" in {
      val returns = VariableReturns(
        Vector(
          VariableReturn("2017.01", 0.1),
          VariableReturn("2017.02", 0.2),
          VariableReturn("2017.03", 0.3)
        )
      )

      Returns.monthlyInterest(0, OffsetReturns(returns, 1)).right.value should === (0.2)
      Returns.monthlyInterest(0, OffsetReturns(returns, 2)).right.value should === (0.3)
    }
  }

  "Returns.fromEquityAndInflationData" should {
    "return the expected collection of VariableReturns objects, each containing correctly-calculated return rates" in {
      /*
        Formula:
          ((currPrice + dividends) / prevPrice) - (currInflation / prevInflation)
       */

      def calcReturn(
                      currPrice: Double,
                      dividends: Double,
                      prevPrice: Double,
                      currInflation: Double,
                      prevInflation: Double
                    ): Double = {
        ((currPrice + dividends / 12) / prevPrice) - (currInflation / prevInflation)
      }

      val equities = Vector(
        EquityData("1900.01", 6.10, 0.22),
        EquityData("1900.02", 6.21, 0.23),
        EquityData("1900.03", 6.26, 0.23),
        EquityData("1900.04", 6.34, 0.24),
        EquityData("1900.05", 6.04, 0.25),
      )

      val inflations = Vector(
        InflationData("1900.01", 7.897),
        InflationData("1900.02", 7.992),
        InflationData("1900.03", 7.992),
        InflationData("1900.04", 7.992),
        InflationData("1900.05", 7.802),
      )

      /*
        Formula:
          ((currPrice + dividends) / prevPrice) - (currInflation / prevInflation)

          def calcReturn(currPrice: Double, dividends: Double, prevPrice: Double, currInflation: Double, prevInflation: Double): Doubl
       */
      val calcOne = calcReturn(6.21, 0.23, 6.10, 7.992, 7.897)
      val calcTwo = calcReturn(6.26, 0.23, 6.21, 7.992, 7.992)
      val calcThree = calcReturn(6.34, 0.24, 6.26, 7.992, 7.992)
      val calcFour = calcReturn(6.04, 0.25, 6.34, 7.802, 7.992)

      val expected = VariableReturns(
        Vector(
          VariableReturn("1900.02", calcOne),
          VariableReturn("1900.03", calcTwo),
          VariableReturn("1900.04", calcThree),
          VariableReturn("1900.05", calcFour)
        )
      )

      Returns.fromEquityAndInflationData(equities, inflations) should === (expected)
    }

  }
}
