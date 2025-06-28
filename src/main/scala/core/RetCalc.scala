package core

import core.RetCalcError.NegativeIncomeError

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
                   ): Either[RetCalcError, Double] = {
    val monthlySavings = netIncome - expenses
    (0 until nMonths).foldLeft(Right(initialCapital): Either[RetCalcError, Double])(
      (capitalPerMonth, month) => {
        for {
          c <- capitalPerMonth
          accumulated <- Returns.monthlyInterest(month, returns)
        } yield c * (1 + accumulated) + monthlySavings
    })
  }



  def simulatePlan(returns: Returns,
                   nMonthsSaving: Int,
                   params: RetCalcParams
                  ): Either[RetCalcError, (Double, Double)] = {

    import params._

    // Calculate capital after savings and interest, before retirement.
    for {
      capitalAtRetirement <- futureCapital(
        initialCapital = initialCapital,
        returns = returns,
        nMonths = nMonthsSaving,
        netIncome = netIncome,
        expenses = expenses
      )

      capitalAtDeath <- futureCapital(
        initialCapital = capitalAtRetirement,
        returns =  OffsetReturns(returns, nMonthsSaving),
        nMonths = nMonthsInRetirement,
        netIncome = 0,
        expenses = expenses
      )

    } yield (capitalAtRetirement, capitalAtDeath)
  }

  def nbMonthsOfSaving(initialCapital: Int,
                       returns: Returns,
                       nMonthsInRetirement: Int,
                       netIncome: Int,
                       expenses: Int): Either[RetCalcError, Int] = {

    @scala.annotation.tailrec
    def go(monthNumber: Int): Either[RetCalcError, Int] = {

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
        case Right((_, capitalAfterDeath)) => {
          if (capitalAfterDeath > 0) {
            Right(monthNumber)
          } else go(monthNumber + 1)
        }
        case Left(err) => Left(err)
      }
    }

    if (expenses > netIncome) {
      Left(NegativeIncomeError(netIncome, expenses))
    } else go(0)
  }


}
