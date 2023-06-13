package sandbox

import sandbox.PrintableInstances.catPrintable

class CodecTest extends RefSpecStyle {
  object `playing with imap` {
    def `it works to encode` = {
       Codec.encode(3.4) shouldBe "3.4"
    }

    def `it works to decode` = {
      Codec.decode[Double]("3.4") shouldBe 3.4
    }
  }
  object `and what about using one more level of abstraction` {
    object `using Box` {
      val myCat = Cat("Jasmine", 3, "calico")
      def `it works to encode` = {
         Codec.encode(Box(myCat)) shouldBe "Jasmine is a 3 year old calico cat."
      }
      def `and it works to decode` = {
        Codec.decode[Box[Cat]]("Jasmine is a 3 year old calico cat.") shouldBe Box(myCat)
      }
    }
  }
}
