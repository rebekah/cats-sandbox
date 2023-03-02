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

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
      def format(cat: Cat): String =
       s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
    }
}
