package chapter_1.cafe

class Cafe {

  val coffee = Coffee(12)

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    (coffee, Charge(cc, coffee.price))
  }

  def buyCoffees(cc: CreditCard, number: Int): (List[Coffee], Charge) = {
    val purchase = List.fill(number)(buyCoffee(cc))

    val (coffee, charge) = purchase.unzip

    coffee -> charge.reduce( _ combine _)
  }


}
