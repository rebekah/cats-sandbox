package sandbox

trait FromString[A] {
  def translate(str: String): A
}

object FromString {
  implicit val fromStringToCat =
    new FromString[Cat] {
      //from s"${cat.name} is a ${cat.age} year old ${cat.color} cat."
      def translate(str: String): Cat = {
        val nameSplit = str.split(" is a ")
        val ageSplit = nameSplit(1).split(" year old ")
        val colorSplit = ageSplit(1).split(" cat.")
        Cat(nameSplit(0), ageSplit(0).toInt, colorSplit(0))
      }
    }

  implicit def fromStringToBox[A](implicit translator: FromString[A]) = new FromString[Box[A]] {
    def translate(str: String): Box[A] = Box(translator.translate(str))
  }
}
