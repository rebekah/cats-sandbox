package sandbox

trait Codec[A] { self =>
  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] =  new Codec[B] {
    def encode(value: B): String = self.encode(enc(value))
    def decode(value: String): B = dec(self.decode(value))
  }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val stringCodec = new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap[Double](_.toDouble, _.toString)

  def strToCat(str: String): Cat = {
    val nameSplit = str.split(" is a ")
    val ageSplit = nameSplit(1).split(" year old ")
    val colorSplit = ageSplit(1).split(" cat.")
    Cat(nameSplit(0), ageSplit(0).toInt, colorSplit(0))
  }

  implicit def catCodec(implicit printable: Printable[Cat]): Codec[Cat] =
    stringCodec.imap[Cat](strToCat(_), printable.format(_))

  implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] =
    codec.imap(Box(_), _.value)
}
