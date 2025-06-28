package core

import scala.io.Source

case class InflationData(month: String, inflation: Double)

object InflationData {
  def fromResource(path: String): Vector[InflationData] = {
    Source.fromResource(path).getLines().drop(1).map {
      str =>
        val fields = str.split("\t")
        val month = fields(0)
        val inflation = fields(1).toDouble

        InflationData(month, inflation)
    }
  }.toVector
}
