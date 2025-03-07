case class Bill(customer: Customer, var items: Map[Item, Int], serviceCharge: Double) {

  private def applyDrinksLoyaltyCardDiscount(): Unit = {
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
            if (card.isNextFree) {
              items = items + (drink -> (quantity - 1)) + (drink.copy(price = 0) -> 1)
            }
            card.addTimestamp()
        }
      case _ => ()
    }
  }

  private def applyDiscountLoyaltyCardDiscount(): Unit = {
    if (subTotal >= 20) {
      customer.loyaltyCard match {
        case Some(card: DiscountLoyaltyCard) =>
          items = items.map({
            case (item, quantity) =>
              val itemDiscount: Double = if (item.category != PremiumMeal) {
                card.getStarCount * 0.02
              } else 0

              item.copy(price = item.price * (1 - itemDiscount)) -> quantity
          })

          card.addStar()
        case _ => ()
      }
    }
  }

  private def subTotal: Double = {
    val itemTotal = items.map({ case (item, quantity) => item.price * quantity }).sum

    itemTotal
  }

  def total: Double = subTotal * (1 + serviceCharge)

  override def toString: String = {
    val itemDetails = items.map({
      case (item, quantity) => s"${item.name} x $quantity x ${item.price}"
    }).mkString("\n")
    f"Customer: ${customer.name}\nItems:\n$itemDetails\nSubtotal: $subTotal%.2f\n" +
      f"Service Charge: $serviceCharge\nTotal: $total%.2f"
  }

  applyDrinksLoyaltyCardDiscount()

  applyDiscountLoyaltyCardDiscount()
}
