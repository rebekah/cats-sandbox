package sandbox

import EqualsInstances._

class EqualsTest extends RefSpecStyle {
  object `for basicIntTest` {
    object `without the interface syntax`{
      def `when the integers are equal` = {
        assert(intEq.eqv(123, 123))
      }

      def `when the integers are not equal` = {
        assert(intEq.neqv(123, 124))
      }
    }
    object `with the interface syntax` {
      import cats.syntax.eq._
      def `when the integers are equal` = {
        //something is off about the === import reference, it's not working.
        //it seems to favor downloading some converter from scalactic
        assert(123.eqv(123))
      }

      def `when the integers are not equal` = {
        assert(123 =!= 124)
      }
    }
  }
  object `for Option tests` {
    import cats.syntax.eq._
    import cats.instances.int._
    import cats.instances.option._
    object `when the syntax for option are not imported`{
      def `comparing some and none typed one way should work` = {
        assert((Some(1): Option[Int]).neqv(None: Option[Int]))
      }
      def `comparing some and none typed another way should also work` = {
        assert(Option(1).neqv(Option.empty[Int]))
      }
    }
    object `when the syntax for option are imported` {
      import cats.syntax.option._
      def `it still works` = {
        assert(1.some.neqv(none[Int]))
      }
    }
  }

  object `from 1.5.4 Comparing Custom Types` {
    object `when I run the code in the book`{
      def `it should work` = {
        import java.util.Date
        import cats.Eq
        import cats.instances.long._ // for Eq
        import cats.syntax.eq._

        implicit val dateEq: Eq[Date] =
          Eq.instance[Date] { (date1, date2) =>
            (date1.getTime).eqv(date2.getTime)
          }

        val x = new Date() // now
        Thread.sleep(1000)
        val y = new Date() // a bit later than now

        assert(x.eqv(x))
        // res12: Boolean = true
        assert(x.neqv(y))
        // res13: Boolean = false
      }
    }

    //exercise 1.5.5
    object `when comparing two cats` {
      import cats.Eq

      val cat1 = Cat(name = "Garfield", age = 4, color = "black and white")
      val cat2 = Cat(name = "Max", age = 3, color = "white")
      val cat3 = Cat(name = "Max", age = 3, color = "white")

      object `without implicit class syntax helpers` {
        val catEqInstance = Eq.apply[Cat]

        def `the same cat should equal itself` = {
          assert(catEqInstance.eqv(cat1, cat1))
        }
        def `two cats with same values should be equal` = {
          assert(catEqInstance.eqv(cat2, cat3))
        }
        def `two cats with different values will not be equal` = {
          assert(catEqInstance.neqv(cat1, cat2))
        }
      }
      object `with implicit class syntax helpers` {
        import cats.syntax.eq._

        def `the same cat should equal itself` = {
          assert(cat1.eqv(cat1))
        }
        def `two cats with same values should be equal` = {
          assert(cat2.eqv(cat3))
        }
        def `two cats with different values will not be equal` = {
          assert(cat1.neqv(cat2))
        }
      }
    }
  }

}
