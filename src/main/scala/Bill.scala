import sttp.client4.quick._
import sttp.client4.Response
import sttp.model.Uri

import java.util.Currency

case class Bill(customer: Customer, var items: Map[Item, Int], serviceCharge: Double, toCurrencyCode: String = "GBP", fromCurrencyCode: String = "GBP") {
  private var toCurrencySymbol: String = Currency.getInstance(fromCurrencyCode.toUpperCase).getSymbol

  val exchangeRate: Double = getExchangeRate

  def getExchangeRate: Double = {
    val exchangeRateOrError: Either[Bill.BillError, Double] = Bill.fetchExchangeRate(toCurrencyCode, fromCurrencyCode)
    val exchangeRate: Double = exchangeRateOrError match {
      case Left(_) =>
        1.0
      case Right(rate) =>
        toCurrencySymbol = Currency.getInstance(toCurrencyCode.toUpperCase).getSymbol
        rate
    }

    exchangeRate
  }

  private def applyExchangeRate(): Unit = {
    items = items.map({
      case (item, quantity) =>
        item.copy(price = item.price * exchangeRate) -> quantity
    })
  }

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
      case (item, quantity) => f"${item.name} x $quantity x $toCurrencySymbol${item.price}%.2f"
    }).mkString("\n")
    f"Customer: ${customer.name}\nItems:\n$itemDetails\nSubtotal: $toCurrencySymbol$subTotal%.2f\n" +
      f"Service Charge: $serviceCharge\nTotal: $toCurrencySymbol$total%.2f"
  }

  applyDrinksLoyaltyCardDiscount()

  applyDiscountLoyaltyCardDiscount()

  applyExchangeRate()
}

object Bill {

  abstract class BillError(message: String) extends Exception(message)

  case object BillErrorToCurrencyNotFound extends BillError("To currency not found")

  case object BillErrorFromCurrencyNotFound extends BillError("From currency not found")

  def fetchExchangeRate(to: String, from: String = "gbp"): Either[BillError, Double] = {
    val uri: Uri = uri"https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@latest/v1/currencies/${from.toLowerCase()}.json"
    try {
      val response: Response[String] = quickRequest.get(uri).send()
      val json = ujson.read(response.body)
      val rates: Map[String, Double] = json(from.toLowerCase()).obj.toMap.map({
        case (key, value) => key -> value.num
      })
      val rate: Option[Double] = rates.get(to.toLowerCase())
      rate match {
        case Some(rate) => Right(rate)
        case None => Left(BillErrorToCurrencyNotFound)
      }
    } catch {
      case _: Exception =>
        Left(BillErrorFromCurrencyNotFound)
    }
  }
}
