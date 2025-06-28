package core

import cats.data.ValidatedNel

abstract class RetCalcError(val message: String) extends Exception(message)

object RetCalcError {
  type RetCalcResult[A] = ValidatedNel[RetCalcError, A]

  case class MonthOutOfBoundsError(month: Int, maximum: Int) extends RetCalcError(
    s"Cannot get returns for month $month. Accepted range: 0 to $maximum."
  )

  case class NegativeIncomeError(income: Int, expenses: Int) extends RetCalcError(
    s"Income $income is greater than expenses of $expenses!"
  )

  case class InvalidArgFormatError(name: String, value: String, expectedFormat: String)
    extends RetCalcError(
      s"Invalid format for argument '$name'. Expected '$expectedFormat'. Found '$value'."
    )

  case class InvalidNumberError(name: String, value: String)
    extends RetCalcError(
      s"Unable to parse number argument '$name' with value provided: '$value'."
    )

  case class MonthIDNotFoundError(monthId: String, minMonthId: String, maxMonthId: String)
    extends RetCalcError(
      s"Unable to locate returns data for monthId $monthId. Minimum month ID: $minMonthId. Maximum monthID: $maxMonthId"
    )
}
