package sandbox

import cats.{Monoid, Semigroup}

import scala.collection.immutable.Set

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
  object `when doing exercise 2.4`{
    object `for custom set monoids` {
      object `for the combine function of monoid` {
        class UnionMonoid[A] {
          val monoid = new Monoid[Set[A]] {
            def combine(x: Set[A], y: Set[A]): Set[A] = {
              x.union(y)
            }

            def empty = Set.empty[A]
          }
        }

        val monoid = new UnionMonoid[Int].monoid

        def `it passes the test for associativity` = {
          assert(
            monoid.combine(Set(5, 6, 7), monoid.combine(Set(1, 2, 3), Set(3, 4, 5))) ==
              monoid.combine(monoid.combine(Set(5, 6, 7), Set(1, 2, 3)), Set(3, 4, 5))
          )
        }

        def `it passes for the identity function for true` = {
          assert(monoid.combine(Set(5, 6, 7), monoid.empty) == Set(5, 6, 7))
        }
      }
    }
    object `for custom Set semigroups` {

      object `for the combine function intersect` {
        class IntersectSemigroup[A] {
          val semigroup = new Semigroup[Set[A]] {
            def combine(x: Set[A], y: Set[A]): Set[A] = {
              x intersect y
            }
          }
        }

        val semigroup = new IntersectSemigroup[Int].semigroup

        def `it passes the test for associativity` = {
          assert(
            semigroup.combine(Set(5, 6, 7), semigroup.combine(Set(1, 2, 3), Set(3, 4, 5))) ==
              semigroup.combine(semigroup.combine(Set(5, 6, 7), Set(1, 2, 3)), Set(3, 4, 5))
          )
        }
      }

      object `for the combine function for symetric diffs` {
        class IntersectSemigroup[A] {
          val semigroup = new Semigroup[Set[A]] {
            def combine(x: Set[A], y: Set[A]): Set[A] = {
              (x diff y) union (y diff x)
            }
          }
        }

        val semigroup = new IntersectSemigroup[Int].semigroup

        def `it passes the test for associativity` = {
          assert(
            semigroup.combine(Set(5, 6, 7), semigroup.combine(Set(1, 2, 3), Set(3, 4, 5))) ==
              semigroup.combine(semigroup.combine(Set(5, 6, 7), Set(1, 2, 3)), Set(3, 4, 5))
          )
        }
      }
    }
  }
}
