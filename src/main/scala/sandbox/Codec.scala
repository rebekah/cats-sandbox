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
    Codec.stringCodec.imap[Double](_.toDouble, _.toString)

  implicit def boxCodec[A](implicit printable: Printable[Box[A]], fromString: FromString[Box[A]]): Codec[Box[A]] =
    Codec.stringCodec.imap(fromString.translate, printable.format)
}
