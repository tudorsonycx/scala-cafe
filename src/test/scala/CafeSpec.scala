import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CafeSpec extends AnyWordSpec with Matchers {
  val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
  val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)
  val item2: Item = Item("Test Item 2", 20.0, HotFood, 5)
  val item3: Item = Item("Test Item 3", 30.0, PremiumMeal, 3)

  val itemInvalid: Item = Item("Invalid Item", 10.0, ColdFood)

  val cafe: Cafe = Cafe("Test Cafe", Menu(List(item0, item1, item2, item3)))

  "Cafe.showMenu" should {
    "return a string representation of the menu" in {
      val expectedResult: String =
        "Test Item 0 (ColdDrink) - 5.0\nTest Item 1 (ColdFood) - 10.0\nTest Item 2 (HotFood) - 20.0\nTest Item 3 (PremiumMeal) - 30.0"

      cafe.showMenu shouldBe expectedResult
    }
  }

  "Cafe.getItemCost" should {
    "return the total cost of an item" when {
      "called with valid item and quantity" in {
        cafe.getItemCost(item2, 2) shouldBe Right(40.0)
      }
    }

    "return an error" when {
      "called with invalid item" in {
        val result: Either[Exception, Double] = cafe.getItemCost(itemInvalid, 1)

        val expected: Left[Exception, Nothing] =
          Left(Cafe.MenuInvalidItemError("Invalid Item not found in menu"))

        result shouldBe expected
      }

      "called with invalid quantity" in {
        val result: Either[Exception, Double] = cafe.getItemCost(item1, 6)

        val expectedResult: Left[Exception, Nothing] =
          Left(Cafe.MenuInvalidQuantityError("Not enough stock for Test Item 1. Available: 5"))

        result shouldBe expectedResult
      }
    }
  }

  "Cafe.updateMenuItem" should {
    "return the updated menu" when {
      "called with valid item and stock" in {
        cafe.updateMenuItem(item1, 10) shouldBe Right(Map(item0 -> 20, item1 -> 10, item2 -> 5, item3 -> 3))
      }
    }

    "return an error" when {
      "called with invalid item" in {
        cafe.updateMenuItem(itemInvalid, 10) shouldBe Left(Cafe.MenuInvalidItemError("Invalid Item not found in menu"))
      }
      "called with negative stock" in {
        cafe.updateMenuItem(item1, -1) shouldBe Left(Cafe.MenuInvalidQuantityError("Stock cannot be negative"))
      }
    }
  }

  val customer: Customer = Customer("John Doe", 35)
  "Cafe.placeOrder" should {
    "return a bill" when {
      "called with valid items list and no service charge" in {
        val cafe: Cafe = Cafe("Test Cafe", Menu(List(item0, item1, item2, item3)))
        val items: Map[Item, Int] = Map(item2 -> 2, item3 -> 3)

        cafe.placeOrder(customer, items, addServiceCharge = false)() match {
          case Right(bill) =>
            bill.customer shouldBe customer
            bill.amount shouldBe 130.0

            cafe.menuWithStock(item2) shouldBe 3
            cafe.menuWithStock(item3) shouldBe 0
        }
      }
      "called with valid items list containing only drinks and automatic service charge" in {
        val cafe: Cafe = Cafe("Test Cafe", Menu(List(item0, item1, item2, item3)))
        val items: Map[Item, Int] = Map(item0 -> 3)

        cafe.placeOrder(customer, items)() match {
          case Right(bill) =>
            bill.customer shouldBe customer
            bill.amount shouldBe 15.0
        }

        cafe.menuWithStock(item0) shouldBe 17
      }

      "called with valid items list containing cold food and automatic service charge" in {
        val cafe: Cafe = Cafe("Test Cafe", Menu(List(item0, item1, item2, item3)))
        val items: Map[Item, Int] = Map(item0 -> 1, item1 -> 1)

        cafe.placeOrder(customer, items)() match {
          case Right(bill) =>
            bill.customer shouldBe customer
            bill.amount shouldBe 16.5
        }

        cafe.menuWithStock(item0) shouldBe 19
        cafe.menuWithStock(item1) shouldBe 4
      }

      "called with valid items list containing hot food and automatic service charge" in {
        val cafe: Cafe = Cafe("Test Cafe", Menu(List(item0, item1, item2, item3)))
        val items: Map[Item, Int] = Map(item2 -> 2)

        cafe.placeOrder(customer, items)() match {
          case Right(bill) =>
            bill.amount shouldBe 48.0
        }

        cafe.menuWithStock(item2) shouldBe 3
      }

      "called with valid items list containing premium meal and automatic service charge" in {
        val cafe: Cafe = Cafe("Test Cafe", Menu(List(item0, item1, item2, item3)))
        val items: Map[Item, Int] = Map(item0 -> 2, item2 -> 2, item3 -> 2)

        cafe.placeOrder(customer, items)() match {
          case Right(bill) =>
            bill.amount shouldBe 137.5
        }
      }

      "called with valid item list and custom service charge" in {
        val cafe: Cafe = Cafe("Test Cafe", Menu(List(item0, item1, item2, item3)))
        val items: Map[Item, Int] = Map(item1 -> 4, item2 -> 1, item3 -> 1)

        cafe.placeOrder(customer, items)(Some(0.05)) match {
          case Right(bill) =>
            bill.amount shouldBe 94.5
        }
      }
    }

    "return an error" when {
      "called with invalid items list" in {
        cafe.placeOrder(customer, Map())() match {
          case Left(e) =>
            e shouldBe Cafe.OrderInvalidItemList("Order cannot be empty")
        }
      }

      "called with list containing invalid item" in {
        cafe.placeOrder(customer, Map(itemInvalid -> 1))() match {
          case Left(e) =>
            e shouldBe Cafe.MenuInvalidItemError("Invalid Item not found in menu")
        }
      }

      "called with list containing item with invalid quantity" in {
        cafe.placeOrder(customer, Map(item3 -> 4))() match {
          case Left(e) =>
            e shouldBe Cafe.MenuInvalidQuantityError("Not enough stock for Test Item 3. Available: 3")
        }
      }
    }
  }
}
