case class Bill(customer: Customer, var items: Map[Item, Int], serviceCharge: Double) {

  private def applyDrinksLoyaltyCardDiscount(): String = {
    customer.loyaltyCard match {
      case Some(card: DrinksLoyaltyCard) =>
        val firstDrink: Option[(Item, Int)] = items.find({
          case (item, _) => item.category match {
            case _: Drink => true
            case _ => false
          }
        })

        firstDrink match {
          case Some((drink, quantity)) =>
            val message: String = if (card.isNextFree) {
              items = items + (drink -> (quantity - 1)) + (drink.copy(price = 0) -> 1)
              s"Free drink applied: ${drink.name}"
            } else {
              s"Free drink not available yet"
            }
            card.addTimestamp()
            message
          case None => s"No drinks in order"
        }
      case None => "No drinks loyalty card"
    }
  }

  applyDrinksLoyaltyCardDiscount()

  private def subTotal: Double = {
    val itemTotal = items.map({ case (item, quantity) => item.price * quantity }).sum

    itemTotal
  }

  def total: Double = subTotal * (1 + serviceCharge)

  override def toString: String = {
    val itemDetails = items.map({
      case (item, quantity) => s"${item.name} x $quantity x ${item.price}"
    }).mkString("\n")
    s"Customer: ${customer.name}\nItems:\n$itemDetails\nSubtotal: $subTotal\n" +
      s"Service Charge: $serviceCharge\nTotal: $total"
  }
}
