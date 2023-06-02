package sandbox

class AddExercisesTest extends RefSpecStyle {
  object `adding two numbers` {
    def `works using Monoids` = {
      AddExercises.addThings(List(3,4,5)) shouldBe 12
    }
  }

  object `adding two Options of type Int` {
    def `works using Monoids` = {
      AddExercises.addThings(List(Option(3), None, Option(5))) shouldBe Option(8)
    }
  }

  object `adding two Orders` {
    def `works using Monoids` = {
      AddExercises.addThings(List(Order(1,2), Order(2,3), Order(3,4))) shouldBe Order(6,9)
    }
  }
}