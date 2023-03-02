package sandbox
import cats.Show

object ShowInstances {
  implicit val catsShow: Show[Cat] = {
    Show.show(cat => s"${cat.name} is a ${cat.age} year old ${cat.color} cat")
  }
}
