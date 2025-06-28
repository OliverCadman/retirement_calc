package core

import cats.data.{NonEmptyList, Validated}
import core.RetCalcError.{InvalidArgFormatError, InvalidNumberError, RetCalcResult}
import cats.implicits._


object ArgParser {

  def parseInt(name: String, value: String): RetCalcResult[Int] =
    Validated.catchOnly[NumberFormatException](value.toInt)
      .leftMap(_ => NonEmptyList.of(InvalidNumberError(name, value)))

  def parseDouble(name: String, value: String): RetCalcResult[Double] =
    Validated.catchOnly[NumberFormatException](value.toDouble)
      .leftMap(_ => NonEmptyList.of(InvalidNumberError(name, value)))

  def parseParams(params: Array[String]): RetCalcResult[RetCalcParams] =

  /*
      val nYearsSaving = args(1).toInt
      val nYearsInRetirement = args(2).toInt
      val netIncome = args(3).toDouble
      val expenses = args(4).toDouble
      val initialCapital = args(5).toDouble
   */
    (
      parseInt("nbYearsInRetirement", params(2)),
      parseDouble("netIncome", params(3)),
      parseDouble("expenses", params(4)),
      parseDouble("initialCapital", params(5))
    ).mapN {
      case (yearsInRetirement, netIncome, expenses, initialCapital) =>
        RetCalcParams(
          initialCapital = initialCapital,
          nMonthsInRetirement = yearsInRetirement * 12,
          netIncome = netIncome,
          expenses = expenses
        )
    }

  def parseFromUntil(fromUntil: String): RetCalcResult[(String, String)] = {
      val parsedFromUntilArgs = fromUntil.split(":")

      if (parsedFromUntilArgs.length <= 1) {
        InvalidArgFormatError("fromUntil", fromUntil, "FROM:UNTIL").invalidNel
      } else (parsedFromUntilArgs(0), parsedFromUntilArgs(1)).validNel
  }
}
