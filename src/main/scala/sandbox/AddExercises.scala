package sandbox

import cats.Monoid
import cats.syntax.semigroup._

//2.5.4
object AddExercises {
  def addThings[T: Monoid](list: List[T]): T = {
    list.foldLeft(Monoid[T].empty)( (inc, thing) =>
      inc |+| thing
    )
  }
}
