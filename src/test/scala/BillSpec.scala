import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate
import scala.annotation.tailrec

class BillSpec extends AnyWordSpec with Matchers {
  final def getNTimestampsList(n: Int): List[LocalDate] = {
    @tailrec
    def iterate(i: Int = 0, acc: List[LocalDate] = List()): List[LocalDate] = {
      if (i == n) {
        acc
      } else {
        iterate(i + 1, acc :+ LocalDate.now().plusDays(n))
      }
    }

    iterate()
  }

  val cafe: Cafe = Cafe("Test Cafe", Menu(List()))

  "Bill.toString" should {
    "return a string representation of the bill" in {
      val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
      val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)

      val customer: Person = Person("Test Customer", 18, None)

      val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1)

      val serviceCharge: Double = 0.1

      val bill: Bill = new Bill(cafe, customer, items, serviceCharge, "usd") {
        override def isHappyHour: Boolean = false
      }

      val exchangeRate: Double = bill.exchangeRate

      val result: String = bill.toString

      result should include("Customer: Test Customer")
      result should include(f"Test Item 0 x 2 x US$$${5.0 * exchangeRate}%.2f")
      result should include(f"Test Item 1 x 1 x US$$${10.0 * exchangeRate}%.2f")
      result should include(f"Subtotal: US$$${20.0 * exchangeRate}%.2f")
      result should include(f"Service Charge: $serviceCharge")
      result should include(f"Total: US$$${22.0 * exchangeRate}%.2f")
    }

    "return a string representation of the bill with drinks discount" in {
      val timestampsMock: List[LocalDate] = getNTimestampsList(9)

      val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
      val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)

      val customer: Person = Person("Test Customer", 18, None, Some(new DrinksLoyaltyCard() {
        timestamps = timestampsMock
      }))

      val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1)

      val serviceCharge: Double = 0.1

      val bill: Bill = new Bill(cafe, customer, items, serviceCharge, "RON") {
        override def isHappyHour: Boolean = false
      }

      val exchangeRate: Double = bill.exchangeRate

      val result: String = bill.toString

      result should include("Customer: Test Customer")
      result should include(f"Test Item 0 x 1 x RON${5.0 * exchangeRate}%.2f")
      result should include(f"Test Item 1 x 1 x RON${10.0 * exchangeRate}%.2f")
      result should include(f"Test Item 0 x 1 x RON${0.0 * exchangeRate}%.2f")
      result should include(f"Subtotal: RON${15.0 * exchangeRate}%.2f")
      result should include(f"Service Charge: $serviceCharge")
      result should include(f"Total: RON${16.5 * exchangeRate}%.2f")

      bill.customer.loyaltyCard match {
        case Some(card: DrinksLoyaltyCard) =>
          card.getTimestampsLength shouldBe 0
      }
    }

    "return a string representation of the bill with discount for non-premium items" when {
      "discount loyalty card has 1 star" in {
        val item0: Item = Item("Test Item 0", 10.0, ColdFood)
        val item1: Item = Item("Test Item 1", 6.0, HotDrink)
        val item2: Item = Item("Test Item 2", 17.0, PremiumMeal)
        val item3: Item = Item("Test Item 3", 5.0, AlcoholicDrink)

        val customer: Person = Person("Test Customer", 18, None, Some(new DiscountLoyaltyCard() {
          timestamps = getNTimestampsList(1)
        }))

        val items: Map[Item, Int] = Map(item0 -> 1, item1 -> 2, item2 -> 1, item3 -> 3)

        val serviceCharge: Double = 0.25

        val bill: Bill = new Bill(cafe, customer, items, serviceCharge, "EUR") {
          override def isHappyHour: Boolean = false
        }

        val exchangeRate: Double = bill.exchangeRate

        val result: String = bill.toString

        result should include("Customer: Test Customer")
        result should include(f"Test Item 0 x 1 x €${9.8 * exchangeRate}%.2f")
        result should include(f"Test Item 1 x 2 x €${5.88 * exchangeRate}%.2f")
        result should include(f"Test Item 2 x 1 x €${17.0 * exchangeRate}%.2f")
        result should include(f"Test Item 3 x 3 x €${4.9 * exchangeRate}%.2f")
        result should include(f"Subtotal: €${53.26 * exchangeRate}%.2f")
        result should include(f"Service Charge: $serviceCharge")
        result should include(f"Total: €${66.575 * exchangeRate}%.2f")


        bill.customer.loyaltyCard match {
          case Some(card: DiscountLoyaltyCard) =>
            card.getStarCount shouldBe 2
        }
      }

      "discount loyalty card has 8 stars" in {
        val item0: Item = Item("Test Item 0", 20.0, HotFood)
        val item1: Item = Item("Test Item 1", 30.0, PremiumMeal)
        val item2: Item = Item("Test Item 2", 25.0, PremiumMeal)
        val item3: Item = Item("Test Item 3", 5.0, AlcoholicDrink)
        val item4: Item = Item("Test Item 4", 35.0, AlcoholicDrink)

        val customer: Person = Person("Test Customer", 18, None, Some(new DiscountLoyaltyCard() {
          timestamps = getNTimestampsList(8)
        }))

        val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1, item2 -> 2, item3 -> 10, item4 -> 2)

        val serviceCharge: Double = 0.15

        val bill: Bill = new Bill(cafe, customer, items, serviceCharge, "JPY") {
          override def isHappyHour: Boolean = false
        }

        val exchangeRate: Double = bill.exchangeRate

        val result: String = bill.toString

        result should include("Customer: Test Customer")
        result should include(f"Test Item 0 x 2 x JP¥${16.8 * exchangeRate}%.2f")
        result should include(f"Test Item 1 x 1 x JP¥${30.0 * exchangeRate}%.2f")
        result should include(f"Test Item 2 x 2 x JP¥${25.0 * exchangeRate}%.2f")
        result should include(f"Test Item 3 x 10 x JP¥${4.2 * exchangeRate}%.2f")
        result should include(f"Test Item 4 x 2 x JP¥${29.4 * exchangeRate}%.2f")
        result should include(f"Subtotal: JP¥${214.4 * exchangeRate}%.2f")
        result should include(f"Service Charge: $serviceCharge")
        result should include(f"Total: JP¥${246.56 * exchangeRate}%.2f")

        bill.customer.loyaltyCard match {
          case Some(card: DiscountLoyaltyCard) =>
            card.getStarCount shouldBe 8
        }
      }

      "discount loyalty card has 8 stars and customer that has worked for at least 6 months there" in {
        val item0: Item = Item("Test Item 0", 20.0, HotFood)
        val item1: Item = Item("Test Item 1", 30.0, PremiumMeal)
        val item2: Item = Item("Test Item 2", 25.0, PremiumMeal)
        val item3: Item = Item("Test Item 3", 5.0, AlcoholicDrink)
        val item4: Item = Item("Test Item 4", 35.0, AlcoholicDrink)

        val sixMonthsAgo = LocalDate.now().minusMonths(6)

        val customer: Person = Person("Test Customer", 18, Some(cafe.jobFactory(sixMonthsAgo)), Some(new DiscountLoyaltyCard() {
          timestamps = getNTimestampsList(8)
        }))

        val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1, item2 -> 2, item3 -> 10, item4 -> 2)

        val serviceCharge: Double = 0.25

        val bill: Bill = new Bill(cafe, customer, items, serviceCharge, "CAD") {
          override def isHappyHour: Boolean = false
        }

        val exchangeRate: Double = bill.exchangeRate

        val result: String = bill.toString

        result should include("Customer: Test Customer")
        result should include(f"Test Item 0 x 2 x CA$$${15.12 * exchangeRate}%.2f")
        result should include(f"Test Item 1 x 1 x CA$$${27.0 * exchangeRate}%.2f")
        result should include(f"Test Item 2 x 2 x CA$$${22.5 * exchangeRate}%.2f")
        result should include(f"Test Item 3 x 10 x CA$$${3.78 * exchangeRate}%.2f")
        result should include(f"Test Item 4 x 2 x CA$$${26.46 * exchangeRate}%.2f")
        result should include(f"Subtotal: CA$$${192.96 * exchangeRate}%.2f")
        result should include(f"Service Charge: $serviceCharge")
        result should include(f"Total: CA$$${241.2 * exchangeRate}%.2f")

        bill.customer.loyaltyCard match {
          case Some(card: DiscountLoyaltyCard) =>
            card.getStarCount shouldBe 8
        }
      }

      "discount loyalty card has 8 stars and time is happy hours" in {
        val item0: Item = Item("Test Item 0", 20.0, HotFood)
        val item1: Item = Item("Test Item 1", 30.0, PremiumMeal)
        val item2: Item = Item("Test Item 2", 25.0, PremiumMeal)
        val item3: Item = Item("Test Item 3", 5.0, AlcoholicDrink)
        val item4: Item = Item("Test Item 4", 35.0, AlcoholicDrink)

        val customer: Person = Person("Test Customer", 18, None, Some(new DiscountLoyaltyCard() {
          timestamps = getNTimestampsList(5)
        }))

        val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1, item2 -> 2, item3 -> 10, item4 -> 2)

        val serviceCharge: Double = 0.15

        val bill: Bill = new Bill(cafe, customer, items, serviceCharge) {
          override def isHappyHour: Boolean = true
        }

        val exchangeRate: Double = bill.exchangeRate

        val result: String = bill.toString

        result should include("Customer: Test Customer")
        result should include(f"Test Item 0 x 2 x £${18.0 * exchangeRate}%.2f")
        result should include(f"Test Item 1 x 1 x £${30.0 * exchangeRate}%.2f")
        result should include(f"Test Item 2 x 2 x £${25.0 * exchangeRate}%.2f")
        result should include(f"Test Item 3 x 10 x £${2.5 * exchangeRate}%.2f")
        result should include(f"Test Item 4 x 2 x £${17.5 * exchangeRate}%.2f")
        result should include(f"Subtotal: £${176.0 * exchangeRate}%.2f")
        result should include(f"Service Charge: $serviceCharge")
        result should include(f"Total: £${202.4 * exchangeRate}%.2f")

        bill.customer.loyaltyCard match {
          case Some(card: DiscountLoyaltyCard) =>
            card.getStarCount shouldBe 6
        }
      }
    }
  }
}