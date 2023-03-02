package sandbox

import cats.Eq
import cats.instances.string._
import cats.Show
import ShowInstances._

object EqualsInstances {
  val intEq = Eq[Int]
  val strEq = Eq[String]
  val showCat = Show[Cat]

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] {
    (cat1: Cat, cat2: Cat) => strEq.eqv(showCat.show(cat1), showCat.show(cat2))
  }
}
