package core

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InflationDataSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {

  "InflationData.fromResource" should {
    "return the expected collection of InflationData case classes based on first 10 rows of cpi.tsv " +
      "file (found in resources)." in {
      val inflations = Vector(
        InflationData("1900.01", 7.897),
        InflationData("1900.02", 7.992),
        InflationData("1900.03", 7.992),
        InflationData("1900.04", 7.992),
        InflationData("1900.05", 7.802),
        InflationData("1900.06", 7.707),
        InflationData("1900.07", 7.802),
        InflationData("1900.08", 7.707),
        InflationData("1900.09", 7.802),
        InflationData("1900.10", 7.707),
      )

      InflationData.fromResource("cpi.tsv").take(10) should === (inflations)
    }
  }
}
