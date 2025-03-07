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

  "Bill.toString" should {
    "return a string representation of the bill" in {
      val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
      val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)
      val customer: Customer = Customer("Test Customer", 18)
      val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1)
      val serviceCharge: Double = 0.1
      val bill: Bill = Bill(customer, items, serviceCharge)

      val result: String = bill.toString

      result should include("Customer: Test Customer")
      result should include("Test Item 0 x 2 x 5.0")
      result should include("Test Item 1 x 1 x 10.0")
      result should include("Subtotal: 20.0")
      result should include(s"Service Charge: $serviceCharge")
      result should include("Total: 22.0")
    }

    "return a string representation of the bill with drinks discount" in {
      val timestampsMock: List[LocalDate] = getNTimestampsList(9)

      val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
      val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)

      val customer: Customer = Customer("Test Customer", 18, Some(new DrinksLoyaltyCard() {
        timestamps = timestampsMock
      }))

      val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1)

      val serviceCharge: Double = 0.1

      val bill: Bill = Bill(customer, items, serviceCharge)

      val result: String = bill.toString

      result should include("Customer: Test Customer")
      result should include("Test Item 0 x 1 x 5.0")
      result should include("Test Item 1 x 1 x 10.0")
      result should include("Test Item 0 x 1 x 0.0")
      result should include("Subtotal: 15.0")
      result should include(s"Service Charge: $serviceCharge")
      result should include("Total: 16.5")

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

        val customer: Customer = Customer("Test Customer", 18, Some(new DiscountLoyaltyCard() {
          timestamps = getNTimestampsList(1)
        }))

        val items: Map[Item, Int] = Map(item0 -> 1, item1 -> 2, item2 -> 1, item3 -> 3)

        val serviceCharge: Double = 0.25

        val bill: Bill = Bill(customer, items, serviceCharge)

        val result: String = bill.toString

        result should include("Customer: Test Customer")
        result should include("Test Item 0 x 1 x 9.8")
        result should include("Test Item 1 x 2 x 5.88")
        result should include("Test Item 2 x 1 x 17.0")
        result should include("Test Item 3 x 3 x 4.9")
        result should include("Subtotal: 53.26")
        result should include(s"Service Charge: $serviceCharge")
        result should include("Total: 66.58")

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

        val customer: Customer = Customer("Test Customer", 18, Some(new DiscountLoyaltyCard() {
          timestamps = getNTimestampsList(8)
        }))

        val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1, item2 -> 2, item3 -> 10, item4 -> 2)

        val serviceCharge: Double = 0.15

        val bill: Bill = Bill(customer, items, serviceCharge)

        val result: String = bill.toString

        result should include("Customer: Test Customer")
        result should include("Test Item 0 x 2 x 16.8")
        result should include("Test Item 1 x 1 x 30.0")
        result should include("Test Item 2 x 2 x 25.0")
        result should include("Test Item 3 x 10 x 4.2")
        result should include("Test Item 4 x 2 x 29.4")
        result should include("Subtotal: 214.4")
        result should include(s"Service Charge: $serviceCharge")
        result should include("Total: 246.56")

        bill.customer.loyaltyCard match {
          case Some(card: DiscountLoyaltyCard) =>
            card.getStarCount shouldBe 8
        }
      }
    }
  }
}
