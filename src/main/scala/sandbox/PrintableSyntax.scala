package sandbox

object PrintableSyntax {
  implicit class PrintableOp[A](value: A) {
    def format(implicit printable: Printable[A]): String = {
      printable.format(value)
    }

    def print(implicit printable: Printable[A]): Unit  = {
      println(format(printable))
    }
  }
}
