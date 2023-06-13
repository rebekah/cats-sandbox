package sandbox

object PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String = value
    }

  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String = value.toString
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
      def format(cat: Cat): String =
        s"${cat.name} is a ${cat.age} year old ${cat.color} cat."
    }

  def mapToCat(catMap: Map[String, String]): Cat = {
    Cat(
      catMap.getOrElse("name", "noName"),
      catMap.getOrElse("age", "0").toInt,
      catMap.getOrElse("color", "unknown")
    )
  }

  implicit def printableCatMap(
    implicit catPrintable: Printable[Cat]
  ): Printable[Map[String, String]] = catPrintable.contramap(mapToCat)

  implicit def printableBox[A](implicit printable: Printable[A]): Printable[Box[A]] = {
    printable.contramap[Box[A]](_.value)
  }
}
