package sandbox

import PrintableInstances._

class PrintableTest extends RefSpecStyle {
  object `It returns the expected name` {
      def `for a Cat`  = {
        val Duke = Cat("Duke", 13, "orange")
        assert(Printable.format(Duke) == "Duke is a 13 year old orange cat.")
      }
  }

  object `contramap works: exercise 3.5.5.1` {
    //see PrintableInstances for implicit methods and type class instances
    val catMap: Map[String, String] = Map("name" -> "Kate", "age" -> "3", "color" -> "grey")

    def `it works` = {
      (printableCatMap).format(catMap) shouldBe "Kate is a 3 year old grey cat."
    }

    def `and it works using the format def on Pritable as well` = {
      Printable.format(catMap) shouldBe "Kate is a 3 year old grey cat."
    }

    object `creating a semi-generic converter` {
      object `when there is the necessary implicit` {
        def `also works` = {
          Printable.format(Box("bob")) shouldBe "bob"
        }
      }
    }
  }
}