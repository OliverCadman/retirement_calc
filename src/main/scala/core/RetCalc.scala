package core

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import core.RetCalcError.{NegativeIncomeError, RetCalcResult}
import cats.implicits._

case class RetCalcParams(
                        initialCapital: Double,
                        nMonthsInRetirement: Int,
                        netIncome: Double,
                        expenses: Double
                        )

object RetCalc {

  def futureCapital(
                   initialCapital: Double,
                   returns: Returns,
                   nMonths: Int,
                   netIncome: Double,
                   expenses: Double
                   ): RetCalcResult[Double] = {
    val monthlySavings = netIncome - expenses
    (0 until nMonths).foldLeft(Right(initialCapital): Either[RetCalcError, Double])(
      (capitalPerMonth, month) => {
        for {
          c <- capitalPerMonth
          accumulated <- Returns.monthlyInterest(month, returns)
        } yield c * (1 + accumulated) + monthlySavings
    }).toValidatedNel
  }

  def simulatePlan(returns: Returns,
                   nMonthsSaving: Int,
                   params: RetCalcParams
                  ): RetCalcResult[(Double, Double)] = {

    import params._

    // Calculate capital after savings and interest, before retirement.
    futureCapital(
      initialCapital = initialCapital,
      returns = returns,
      nMonths = nMonthsSaving,
      netIncome = netIncome,
      expenses = expenses
    ).andThen {
      capitalAtRetirement => {
        futureCapital(
          initialCapital = capitalAtRetirement,
          returns =  OffsetReturns(returns, nMonthsSaving),
          nMonths = nMonthsInRetirement,
          netIncome = 0,
          expenses = expenses
        )
      }.map {
        capitalAtDeath => (capitalAtRetirement, capitalAtDeath)
      }
    }
  }

  def nbMonthsOfSaving(initialCapital: Int,
                       returns: Returns,
                       nMonthsInRetirement: Int,
                       netIncome: Int,
                       expenses: Int): RetCalcResult[Int] = {

    @scala.annotation.tailrec
    def go(monthNumber: Int): RetCalcResult[Int] = {

      simulatePlan(
        returns = returns,
        nMonthsSaving = monthNumber,
        params = RetCalcParams(
          initialCapital = initialCapital,
          nMonthsInRetirement = nMonthsInRetirement,
          netIncome = netIncome,
          expenses = expenses
        )
      ) match {
        case Valid((_, capitalAfterDeath)) => {
          if (capitalAfterDeath > 0) {
            Valid(monthNumber)
          } else go(monthNumber + 1)
        }
        case Invalid(err) => Invalid(err)
      }
    }

    if (expenses > netIncome) {
      Invalid(NonEmptyList.of(NegativeIncomeError(netIncome, expenses)))
    } else go(0)
  }


}
