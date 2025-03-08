import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.BeforeAndAfterEach

class CafeSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {
  val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
  val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)
  val item2: Item = Item("Test Item 2", 20.0, HotFood, 5)
  val item3: Item = Item("Test Item 3", 30.0, PremiumMeal, 3)

  var cafe: Cafe = _

  val customer: Person = Person("Test Customer", 18, None)

  override def beforeEach(): Unit = {
    cafe = Cafe("Test Cafe", Menu(List(item0, item1, item2, item3)))
  }

  "Cafe.showMenu" should {
    "return a string representation of the menu" in {
      val expectedResult: String =
        "Test Item 0 (ColdDrink) - 5.0\nTest Item 1 (ColdFood) - 10.0\nTest Item 2 (HotFood) - 20.0\nTest Item 3 (PremiumMeal) - 30.0"

      cafe.showMenu shouldBe expectedResult
    }
  }

  "Cafe.placeOrder" should {
    "return a bill" when {
      "order contains valid items" in {
        val result = cafe.placeOrder(customer, Map("Test Item 0" -> 1))()

        result match {
          case Right(bill) =>
            bill.items should contain(item0 -> 1)
            bill.total shouldBe 5.0
        }
      }

      "order contains multiple valid items" in {
        val result = cafe.placeOrder(customer, Map("Test Item 0" -> 2, "Test Item 1" -> 1))()

        result match {
          case Right(bill) =>
            bill.items should contain(item0 -> 2)
            bill.items should contain(item1 -> 1)
            bill.total shouldBe 22.0
        }
      }
    }

    "apply correct service charge" when {
      "order contains cold drink only" in {
        val result = cafe.placeOrder(customer, Map("Test Item 0" -> 1))()

        result match {
          case Right(bill) =>
            bill.serviceCharge shouldBe 0
            bill.total shouldBe 5.0
        }
      }

      "order contains cold food" in {
        val result = cafe.placeOrder(customer, Map("Test Item 1" -> 1))()

        result match {
          case Right(bill) =>
            bill.serviceCharge shouldBe 0.1
            bill.total shouldBe 11.0
        }
      }

      "order contains hot food" in {
        val result = cafe.placeOrder(customer, Map("Test Item 2" -> 1))()
        result match {
          case Right(bill) =>
            bill.serviceCharge shouldBe 0.2
            bill.total shouldBe 24.0
        }
      }

      "order contains premium meal" in {
        val result = cafe.placeOrder(customer, Map("Test Item 3" -> 1))()

        result match {
          case Right(bill) =>
            bill.serviceCharge shouldBe 0.25
            bill.total shouldBe 37.5
        }
      }

      "order contains items of different categories (should apply highest)" in {
        val result = cafe.placeOrder(customer, Map("Test Item 0" -> 1, "Test Item 3" -> 1))()

        result match {
          case Right(bill) =>
            bill.serviceCharge shouldBe 0.25
            bill.total shouldBe 43.75
        }
      }

      "service charge is disabled" in {
        val result = cafe.placeOrder(customer, Map("Test Item 3" -> 1), addServiceCharge = false)()

        result match {
          case Right(bill) =>
            bill.serviceCharge shouldBe 0
            bill.total shouldBe 30.0
        }
      }

      "custom service charge is provided" in {
        val result = cafe.placeOrder(customer, Map("Test Item 0" -> 1))(Some(0.15))

        result match {
          case Right(bill) =>
            bill.serviceCharge shouldBe 0.15
            bill.total shouldBe 5.75
        }
      }
    }

    "return an error" when {
      "order is empty" in {
        val result = cafe.placeOrder(customer, Map())()

        result shouldBe Left(Cafe.OrderInvalidItemList("Order cannot be empty"))
      }

      "item doesn't exist in menu" in {
        val result = cafe.placeOrder(customer, Map("Invalid Item" -> 1))()
        result shouldBe Left(Cafe.MenuUnavailableItemError("Invalid Item not available"))
      }

      "item quantity exceeds available stock" in {
        val result = cafe.placeOrder(customer, Map("Test Item 1" -> 10))()

        result shouldBe Left(Cafe.MenuUnavailableItemError("Test Item 1 not available"))
      }

      "negative custom service charge is provided" in {
        val result = cafe.placeOrder(customer, Map("Test Item 0" -> 1))(Some(-0.1))

        result shouldBe Left(Cafe.OrderInvalidServiceCharge("Service charge cannot be negative"))
      }
    }

    "handle multiple orders correctly" when {
      "placing orders sequentially" in {
        // First order
        val firstOrder = cafe.placeOrder(customer, Map("Test Item 1" -> 2))()
        firstOrder.isRight shouldBe true

        // Second order - should fail as we only have 3 items left from initial 5
        val secondOrder = cafe.placeOrder(customer, Map("Test Item 1" -> 4))()
        secondOrder.isLeft shouldBe true

        // Third order - should succeed as we have 3 items left
        val thirdOrder = cafe.placeOrder(customer, Map("Test Item 1" -> 3))()
        thirdOrder.isRight shouldBe true

        // Fourth order - should fail as stock is now 0
        val fourthOrder = cafe.placeOrder(customer, Map("Test Item 1" -> 1))()
        fourthOrder.isLeft shouldBe true
      }
    }
  }
}
