package core

import cats.data.NonEmptyList
import cats.implicits._
import core.RetCalcError.{MonthIDNotFoundError, MonthOutOfBoundsError, RetCalcResult}

sealed trait Returns

case class FixedReturn(annualInterest: Double) extends Returns

case class VariableReturn(monthId: String, monthlyInterest: Double)
case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {



  def fromUntil(from: String, until: String): RetCalcResult[VariableReturns] = {
    val minMonthId = returns.head.monthId
    val maxMonthId = returns(returns.length - 1).monthId

    (from, until) match {
      case (from, _) if !returns.exists(_.monthId == from) =>
        MonthIDNotFoundError(from, minMonthId, maxMonthId).invalidNel
      case (_, until) if !returns.exists(_.monthId == until) =>
        MonthIDNotFoundError(until, minMonthId, maxMonthId).invalidNel
      case _ =>
        VariableReturns(
          returns.
            dropWhile(_.monthId != from)
            .takeWhile(_.monthId != until)
        ).validNel
    }
  }
}
case class OffsetReturns(origReturns: Returns, offset: Int) extends Returns

object Returns {

  @scala.annotation.tailrec
  def monthlyInterest(month: Int, returns: Returns): Either[RetCalcError, Double] = returns match {
    case FixedReturn(annualInterest) => Right(annualInterest / 12)
    case VariableReturns(returns) => {
      if (returns.isDefinedAt(month)) {
        Right(returns(month % returns.size).monthlyInterest)
      } else Left(
        MonthOutOfBoundsError(month, returns.length - 1)
      )
    }
    case OffsetReturns(returns, offset) => monthlyInterest(month + offset, returns)
  }

  def fromEquityAndInflationData(equities: Vector[EquityData],
                                 inflations: Vector[InflationData]
                                ): VariableReturns = {
    VariableReturns(
      equities.zip(inflations).sliding(2).map {
        case prevData +: currData +: Vector() => {

          val realInflation = currData._2.inflation / prevData._2.inflation
          val realReturns = ((currData._1.sp500 + currData._1.dividend / 12) / prevData._1.sp500) - realInflation

          VariableReturn(currData._1.month, realReturns)
        }
      }.toVector
    )
  }
}
