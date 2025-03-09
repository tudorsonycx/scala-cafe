import sttp.client4.quick._
import sttp.client4.Response
import sttp.model.Uri

import java.time.{LocalDate, LocalDateTime, LocalTime, Period}
import java.util.Currency

case class Bill(
  cafe: Cafe,
  customer: Person,
  employee: Person,
  var items: Map[Item, Int],
  serviceCharge: Double,
  transactionType: Transaction,
  toCurrencyCode: String = "GBP",
  fromCurrencyCode: String = "GBP") {
  private var toCurrencySymbol: String = Currency.getInstance(fromCurrencyCode.toUpperCase).getSymbol

  val exchangeRate: Double = getExchangeRate

  val transactionDate: LocalDateTime = getTransactionDate

  private def getTransactionDate: LocalDateTime = LocalDateTime.now()

  def isHappyHour: Boolean = {
    val currentTime: LocalTime = LocalTime.now()
    val happyHourStart: LocalTime = LocalTime.of(18, 0)
    val happyHourEnd: LocalTime = LocalTime.of(19, 0)

    !currentTime.isBefore(happyHourStart) && currentTime.isBefore(happyHourEnd)
  }

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

  private def applyHappyHourDiscount(): Unit = {
    if (isHappyHour) {
      items = items.map({
        case (item, quantity) => item.category match {
          case _: Drink => item.copy(price = item.price * 0.5) -> quantity
          case _ => item -> quantity
        }
      })
    }
  }

  private def applyEmployeeDiscount(): Unit = {
    customer.job match {
      case Some(job) =>
        job match {
          case Cafe.CafeJob(joinedDate, place) =>
            if (place == cafe) {
              val monthsWorked: Int = Period.between(joinedDate, LocalDate.now()).getMonths
              if (monthsWorked >= 6) {
                items = items.map({
                  case (item, quantity) => item.category match {
                    case _: Drink if isHappyHour => item -> quantity
                    case _ => item.copy(price = item.price * 0.9) -> quantity
                  }
                })
              }
            }
          case _ => ()
        }
      case None => ()
    }
  }

  private def applyDrinksLoyaltyCardDiscount(): Unit = {
    if (!isHappyHour) {
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
            case None => ()
          }
        case _ => ()
      }
    }
  }

  private def applyDiscountLoyaltyCardDiscount(): Unit = {
    if (subTotal >= 20) {
      customer.loyaltyCard match {
        case Some(card: DiscountLoyaltyCard) =>
          items = items.map({
            case (item, quantity) =>
              val itemDiscount: Double = item.category match {
                case PremiumMeal => 0
                case _: Drink if isHappyHour => 0
                case _ => card.getStarCount * 0.02
              }

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
    f"${cafe.name}\nStaff member: ${employee.name}\nTransaction type: $transactionType\n$transactionDate\n" +
      f"Customer: ${customer.name}\nItems:\n$itemDetails\nSubtotal: $toCurrencySymbol$subTotal%.2f\n" +
      f"Service Charge: $serviceCharge\nTotal: $toCurrencySymbol$total%.2f"
  }

  applyDiscountLoyaltyCardDiscount()

  applyEmployeeDiscount()

  applyHappyHourDiscount()

  applyDrinksLoyaltyCardDiscount()

  applyExchangeRate()
}

object Bill extends App {

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
