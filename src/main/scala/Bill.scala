import sttp.client4.quick._
import sttp.client4.Response
import sttp.model.Uri


case class Bill(customer: Customer, var items: Map[Item, Int], serviceCharge: Double, fromCurrencyCode: String, toCurrencyCode: String = "gbp") {
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

object Bill extends App {

  def fetchExchangeRate(from: String, to: String = "gbp"): Double = {
    val uri: Uri = uri"https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@latest/v1/currencies/"
    try {
      val response: Response[String] = quickRequest.get(uri"$uri${from.toLowerCase()}.json").send()
      val json = ujson.read(response.body)
      val rates: Map[String, Double] = json(from).obj.toMap.map({
        case (key, value) => key -> value.num
      })
      rates.getOrElse(to, 1.0)
    } catch {
      case _: Exception =>
        1.0
    }
  }
}
