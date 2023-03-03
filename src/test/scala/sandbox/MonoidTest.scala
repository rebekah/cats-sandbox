package sandbox

import cats.Monoid

class MonoidTest extends RefSpecStyle {
  object `looking at monoid` {
    object `when working with the default instance for Int` {
      val intMonoid = Monoid[Int]
      def `it has the expected combine function` = {
        assert(intMonoid.combine(1,2) == 3)
      }
      def `its empty value is zero`= {
        assert(intMonoid.empty == 0)
      }
    }
  }
  object `when doing exercise 2.3` {
    object `for custom boolean monoids` {
      object `with the combine of &` {
        object `and the empty of true` {
          val booleanAndMonoid: Monoid[Boolean] = {
            new Monoid[Boolean]{
              def combine(x: Boolean, y: Boolean): Boolean = {
                x && y
              }
              def empty: Boolean = true
            }
          }
          def `it passes the test for associativity` = {
            assert(
              booleanAndMonoid.combine(true, booleanAndMonoid.combine(false, true)) ==
              booleanAndMonoid.combine(booleanAndMonoid.combine(true,false), true)
            )
          }
          def `it passes for the identity function for true` = {
            assert( booleanAndMonoid.combine(true, booleanAndMonoid.empty) == true )
          }
          def `it passes for the identity function for flse` = {
            assert(booleanAndMonoid.combine(false, booleanAndMonoid.empty) == false)
          }
        }
      }
      object `with the combine of |`{
        object `with the empty of false` {
          val booleanAndMonoid: Monoid[Boolean] = {
            new Monoid[Boolean] {
              def combine(x: Boolean, y: Boolean): Boolean = {
                x || y
              }

              def empty: Boolean = false
            }
          }

          def `it passes the test for associativity` = {
            assert(
              booleanAndMonoid.combine(true, booleanAndMonoid.combine(false, true)) ==
                booleanAndMonoid.combine(booleanAndMonoid.combine(true, false), true)
            )
          }

          def `it passes for the identity function for true` = {
            assert(booleanAndMonoid.combine(true, booleanAndMonoid.empty) == true)
          }

          def `it passes for the identity function for flse` = {
            assert(booleanAndMonoid.combine(false, booleanAndMonoid.empty) == false)
          }
        }
      }
      object `with the combine of exclusive` {
        object `with the empty of true` {
          val booleanAndMonoid: Monoid[Boolean] = {
            new Monoid[Boolean] {
              def combine(x: Boolean, y: Boolean): Boolean = {
                (!x && y) || (x && !y)
              }

              def empty: Boolean = false
            }
          }

          def `it passes the test for associativity` = {
            assert(
              booleanAndMonoid.combine(true, booleanAndMonoid.combine(false, true)) ==
                booleanAndMonoid.combine(booleanAndMonoid.combine(true, false), true)
            )
          }

          def `it passes for the identity function for true` = {
            assert(booleanAndMonoid.combine(true, booleanAndMonoid.empty) == true)
          }

          def `it passes for the identity function for flse` = {
            assert(booleanAndMonoid.combine(false, booleanAndMonoid.empty) == false)
          }
        }
      }
      object `with the combine of exclusive nor` {
        object `with the empty of false` {
          val booleanAndMonoid: Monoid[Boolean] = {
            new Monoid[Boolean] {
              def combine(x: Boolean, y: Boolean): Boolean = {
                (!x || y) && (x || !y)
              }

              def empty: Boolean = true
            }
          }

          def `it passes the test for associativity` = {
            assert(
              booleanAndMonoid.combine(true, booleanAndMonoid.combine(false, true)) ==
                booleanAndMonoid.combine(booleanAndMonoid.combine(true, false), true)
            )
          }

          def `it passes for the identity function for true` = {
            assert(booleanAndMonoid.combine(true, booleanAndMonoid.empty) == true)
          }

          def `it passes for the identity function for flse` = {
            assert(booleanAndMonoid.combine(false, booleanAndMonoid.empty) == false)
          }
        }
      }
    }
  }
}
