package sandbox

import PrintableInstances._

class PrintableTest extends RefSpecStyle {
  object `It returns the expected name` {
      def `for a Cat`  = {
        val Duke = Cat("Duke", 13, "orange")
        assert(Printable.format(Duke) == "Duke is a 13 year-old orange cat.")
      }
  }
}