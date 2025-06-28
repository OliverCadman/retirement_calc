package core

import scala.io.Source

case class EquityData(month: String, sp500: Double, dividend: Double)

object EquityData {
  def fromResource(path: String): Vector[EquityData] = {
    Source.fromResource(path).getLines().drop(1).map {
      str =>
        val fields = str.split("\t")
        val month = fields(0)
        val sp500 = fields(1).toDouble
        val dividend = fields(2).toDouble

        EquityData(month, sp500, dividend)
    }
  }.toVector
}
