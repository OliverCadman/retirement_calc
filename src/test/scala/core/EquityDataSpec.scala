package core

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EquityDataSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {

  "EquityData.fromResource" should {
    "return the expected collection of 10 EquityData case classes after loading sp500.tsv from resources" in {
      val equities = Vector(
        EquityData("1900.01", 6.10, 0.22),
        EquityData("1900.02", 6.21, 0.23),
        EquityData("1900.03", 6.26, 0.23),
        EquityData("1900.04", 6.34, 0.24),
        EquityData("1900.05", 6.04, 0.25),
        EquityData("1900.06", 5.86, 0.26),
        EquityData("1900.07", 5.86, 0.26),
        EquityData("1900.08", 5.94, 0.27),
        EquityData("1900.09", 5.80, 0.28),
        EquityData("1900.10", 6.01, 0.29)
      )
      EquityData.fromResource("sp500.tsv").take(10) should === (equities)
    }
  }
}
