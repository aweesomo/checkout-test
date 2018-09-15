package checkout

case class Item(name: String, price: BigDecimal, nthFree: Integer)

object Items {
  val apple = Item("apple", .6, 3)
  val orange = Item("orange", .25, 3)
}

object Parser {
  def parse(names: Seq[String]) = {
    names.map(name => name.toLowerCase).map {
      case Items.apple.name => Items.apple
      case Items.orange.name => Items.orange
      case unknown => throw new IllegalArgumentException(s"Invalid item name $unknown")
    }
  }
}

object PriceCalculator {
  def calculatePrice(items: Seq[Item]) = {
    items.map(item => item.price)
      .sum
  }
}

object Checkout extends App {
  val total = Parser.parse(args)
    .map(e => e.price)
    .sum
  print(total.formatted("%.2f"))
}