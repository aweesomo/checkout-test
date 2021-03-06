package checkout

import org.scalatest.FunSuite


class CheckoutTest extends FunSuite {

  val params = List(
    ("No items", Seq(), 0.0),
    ("One apple", Seq(Items.apple), 0.6),
    ("One orange", Seq(Items.orange), 0.25),
    ("An apple and an orange", Seq(Items.apple, Items.orange), 0.85),
    ("An orange and an apple", Seq(Items.orange, Items.apple), 0.85),
    ("Three oranges", Seq(Items.orange, Items.orange, Items.orange), 0.75),
    ("Three apple", Seq(Items.apple, Items.apple, Items.apple), 1.80)
  )

  params.foreach { i =>
    test(s"Given ${i._1} price should be ${i._3}") {
      assert(PriceCalculator.calculatePrice(i._2) === i._3)
    }
  }

}

class ItemParserTest extends FunSuite {

  val nonExceptionCases = List(
    (Seq("apple"), Seq(Items.apple)),
    (Seq("APPLE"), Seq(Items.apple)),
    (Seq("ApPlE"), Seq(Items.apple)),
    (Seq("orange"),Seq(Items.orange)),
    (Seq("ORANGE"),Seq(Items.orange)),
    (Seq("OrAnGe"), Seq(Items.orange)),
    (Seq("apple", "orange"), Seq(Items.apple, Items.orange)),
    (Seq("apple", "apple"), Seq(Items.apple, Items.apple)),
    (Seq("orange", "orange"), Seq(Items.orange, Items.orange))
  )

  nonExceptionCases.foreach { i =>
    test(s"When parsing ${i._1} parsed items should be ${i._2}") {
      assert(Parser.parse(i._1) === i._2)
    }
  }

  val exceptionCases = List(
    (Seq(""), "Invalid item name "),
    (Seq("egg"), "Invalid item name egg"),
    (Seq("apple", "apple", "egg", "orange", "orange"), "Invalid item name egg")
  )

  exceptionCases.foreach { i =>
    test(s"When parsing ${i._1} exception should be thrown with message ${i._2}") {
      val thrown = intercept[IllegalArgumentException] {
        Parser.parse(i._1)
      }
      assert(thrown.getMessage === i._2)
    }
  }
}

class DiscounterTest extends FunSuite {
  val params = List(
    (Items.orange, 1, 0.25),
    (Items.orange, 2, 0.5),
    (Items.orange, 3, 0.5),
    (Items.orange, 4, 0.75),
    (Items.orange, 5, 1),
    (Items.orange, 6, 1),
    (Items.apple, 1, 0.6),
    (Items.apple, 2, 0.6),
    (Items.apple, 3, 1.2),
    (Items.apple, 4, 1.2),
    (Items.apple, 5, 1.8),
    (Items.apple, 6, 1.8)
  )

  params.foreach { i =>
    test(s"Given ${i._2} ${i._1.name} discounted prict should be ${i._3}") {
      assert(Discounter.applyDiscount(i._2, i._1) === i._3)
    }
  }


}

