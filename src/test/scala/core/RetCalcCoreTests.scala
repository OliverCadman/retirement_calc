package core

import core.RetCalcError.NegativeIncomeError
import org.scalactic.{Equality, TypeCheckedTripleEquals}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalactic.TolerantNumerics.tolerantDoubleEquality
import org.scalatest.EitherValues

class RetCalcCoreTests extends AnyWordSpec with Matchers with TypeCheckedTripleEquals with EitherValues {

  implicit val doubleEquality: Equality[Double] = tolerantDoubleEquality(0.0001)

  val params: RetCalcParams = RetCalcParams(
    initialCapital = 10000,
    nMonthsInRetirement = 40 * 12,
    netIncome = 3000,
    expenses = 1000
  )

  "RetCalc.futureCapital" should {
    "calculate the expected amount saved after a given period using Fixed Return" in {

      // 725996.1871
      val actual = RetCalc.futureCapital(
        initialCapital = 1000,
        returns = FixedReturn(0.04),
        netIncome = 2000,
        nMonths = 24 * 12,
        expenses = 500
      )

      val expected = 725996.1871

      actual.right.value should === (expected)

    }
    "calculate the expected decremented value after 'retirement' using Fixed Return" in {

      // 2995346.0133
      val actual = RetCalc.futureCapital(
        initialCapital = 725996,
        nMonths = 12 * 40,
        netIncome = 0,
        expenses = 500,
        returns = FixedReturn(0.04)
      )

      val expected = 2995346.0133
      actual.right.value should === (expected)
    }
  }

  "RetCalc.simulatePlan" should {
    "calculate the expected amount saved after a given period, then " +
      "calculate the expected decremented value after 'retirement', using Fixed Returns" in {

      val result = RetCalc.simulatePlan(
        returns = FixedReturn(0.04),
        nMonthsSaving = 24 * 12,
        params = params
      )

      result.right.map(_._1).right.value should === (990593.5125)
      result.right.map(_._2).right.value should === (3711442.9572)
    }

    "calculate the expected amounts for accumulation and drawdown" in {
      val nMonthsSaving = 24 * 12
      val returns = VariableReturns(
        Vector.tabulate(nMonthsSaving + params.nMonthsInRetirement)(i => {
          if (i < nMonthsSaving) {
            VariableReturn(i.toString, 0.003)
          } else VariableReturn(i.toString, 0.0025)
        })
      )

      val result = RetCalc.simulatePlan(
        nMonthsSaving = nMonthsSaving,
        returns = returns,
        params = params
      )

      result.right.map(_._1).right.value should === (936739.2915)
      result.right.map(_._2).right.value should === (2179370.5929)
    }
  }

  "RetCalc.nbMonthsOfSaving" should {
    "calculate the expected number of months to save enough and" +
      "capital to retire without risking running out of money, using Fixed Returns" in {
      val result = RetCalc.nbMonthsOfSaving(
        initialCapital = 10000,
        returns = FixedReturn(0.04),
        nMonthsInRetirement = 40 * 12,
        netIncome = 3000,
        expenses = 2000
      )

      val actual = 23 * 12 + 1

      result should === (Right(actual))
    }
    "be able to compute a large result quickly" in {
      val result = RetCalc.nbMonthsOfSaving(
        initialCapital = 1000,
        returns = FixedReturn(0.01),
        netIncome = 3000,
        expenses = 2999,
        nMonthsInRetirement = 80 * 12
      )

      println(result)
    }

    "return None if expenses is greater than net income" in {
      val result = RetCalc.nbMonthsOfSaving(
        initialCapital = 10000,
        returns = FixedReturn(0.04),
        nMonthsInRetirement = 40 * 12,
        netIncome = 1000,
        expenses = 1001
      )

      result should === (Left(NegativeIncomeError(1000, 1001)))
    }
  }

}
