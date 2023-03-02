package sandbox

import ShowInstances._

class ShowTest extends RefSpecStyle {
  val myCat: Cat = Cat("bobbi", 12, "orange")
  object `When using the Show Trait for the custom type Cat` {
    object `without the interface syntax` {
      import cats.Show
      def `it works` = {
        val showCat: Show[Cat] = Show.apply[Cat]
        println(showCat.show(myCat))
        assert(showCat.show(myCat) == "bobbi is a 12 year old orange cat")
      }
    }

    object `with the interface syntax` {
      import cats.syntax.show._
      def `it works` = {
        assert(myCat.show == "bobbi is a 12 year old orange cat")
      }
    }
  }
}
