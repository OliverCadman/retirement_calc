package app

import cats.data.Validated
import core.RetCalcError.RetCalcResult
import core.{ArgParser, EquityData, InflationData, OutputFormatter, RetCalc, RetCalcParams, Returns}
import cats.implicits._

object SimulatePlanApp extends OutputFormatter {

  def strSimulatePlan(returns: Returns, nYearsSaving: Int, params: RetCalcParams): RetCalcResult[String] = {

    import params._

    RetCalc.simulatePlan(
      returns = returns,
      nMonthsSaving = nYearsSaving * 12,
      params = RetCalcParams(
        initialCapital = initialCapital,
        nMonthsInRetirement = nMonthsInRetirement,
        netIncome = netIncome,
        expenses = expenses
      )
    ).map {
      case (capitalAfterSaving, capitalAfterRetirement) =>
        val nYearsInRetirement = nMonthsInRetirement / 12

        val normalisedCapitalAfterSaving = normaliseCurrency(capitalAfterSaving.round.toString)
        val normalisedCapitalAfterRetirement = normaliseCurrency(capitalAfterRetirement.round.toString)
        s"""
           |Capital after ${nYearsSaving} years of saving: £${normalisedCapitalAfterSaving}.
           |Capital after ${nYearsInRetirement} years in retirement: £${normalisedCapitalAfterRetirement}.
           |""".stripMargin
      }.toValidatedNel
    }


  def strMain(args: Array[String]): Validated[String, String] = {

    if (args.length != 6) {
      s"""
        |Not all arguments provided. Expected 6 arguments. Found ${args.length}.
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
        |""".stripMargin.invalid
    } else {
      val equities = EquityData.fromResource("sp500.tsv")
      val inflations = InflationData.fromResource("cpi.tsv")
      val returns = Returns.fromEquityAndInflationData(equities, inflations)

      val validatedFromUntil = ArgParser.parseFromUntil(args(0))
      val validatedYearsSaving = ArgParser.parseInt("nbYearsSaving", args(1))
      val validatedParams = ArgParser.parseParams(args)

      (
        validatedFromUntil,
        validatedYearsSaving,
        validatedParams
      ).tupled
        .andThen {
          case ((from, until), yearsSaving, params) =>
              returns.fromUntil(from, until).andThen { returns =>
                strSimulatePlan(returns, yearsSaving, params)
              }
        }.leftMap(nel => nel.map(_.message).toList.mkString("\n"))
    }
  }

  def main(args: Array[String]): Unit = println(strMain(args))
}
