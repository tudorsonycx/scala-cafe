import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MenuSpec extends AnyWordSpec with Matchers {
  val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
  val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)
  val item2: Item = Item("Test Item 2", 20.0, HotFood, 5)
  val item3: Item = Item("Test Item 3", 30.0, PremiumMeal, 3)

  val menu: Menu = Menu(List(item0, item1, item2, item3))

  "Menu.toString" should {
    "return a string representation of the menu" in {

      menu.toString shouldBe "Test Item 0 (ColdDrink) - 5.0\nTest Item 1 (ColdFood) - 10.0\nTest Item 2 (HotFood) - 20.0\nTest Item 3 (PremiumMeal) - 30.0"
    }
  }

  "Menu.updateMenuItem" should {
    "return the updated menu" when {
      "called with valid item and stock" in {
        menu.updateMenuItem("Test Item 1", 10) shouldBe Right(Map(
          "Test Item 0" -> 20,
          "Test Item 1" -> 10,
          "Test Item 2" -> 5,
          "Test Item 3" -> 3
        ))
      }
    }

    "return an error" when {
      "called with invalid item" in {
        menu.updateMenuItem("Invalid Item", 10) shouldBe Left(Cafe.MenuInvalidItemError("Invalid Item not found in menu"))
      }
      "called with negative stock" in {
        menu.updateMenuItem("Test Item 1", -1) shouldBe Left(Cafe.MenuInvalidQuantityError("Stock cannot be negative"))
      }
    }
  }

  "Menu.isItemAvailable" should {
    "return false" when {
      "the item is not on the menu" in {
        menu.isItemAvailable("Invalid Item", 10) shouldBe false
      }
    }
  }

  "Menu.getItemByName" should {
    "return the item" when {
      "the item is on the menu" in {
        menu.getItemByName("Test Item 1") shouldBe Some(item1)
      }
    }

    "return None" when {
      "the item is not on the menu" in {
        menu.getItemByName("Invalid Item") shouldBe None
      }
    }
  }

  "Menu.updateMenuAfterOrder" should {
    "return an error" when {
      "the order is empty" in {
        menu.updateMenuAfterOrder(Map()) shouldBe Left(Cafe.OrderInvalidItemList("Item map cannot be empty"))
      }
      "the item is not on the menu" in {
        menu.updateMenuAfterOrder(Map("Invalid Item" -> 10)) shouldBe Left(Cafe.MenuUnavailableItemError("Invalid Item not available"))
      }
      "the item is out of stock" in {
        menu.updateMenuAfterOrder(Map("Test Item 3" -> 10)) shouldBe Left(Cafe.MenuUnavailableItemError("Test Item 3 not available"))
      }

      "quantity is negative or 0" in {
        menu.updateMenuAfterOrder(Map("Test Item 1" -> 0)) shouldBe Left(Cafe.MenuInvalidQuantityError("Order quantity cannot be negative"))

        menu.updateMenuAfterOrder(Map("Test Item 1" -> -2)) shouldBe Left(Cafe.MenuInvalidQuantityError("Order quantity cannot be negative"))
      }
    }

    "return a success message" when {
      "the order is valid" in {
        menu.updateMenuAfterOrder(Map("Test Item 0" -> 1, "Test Item 1" -> 1)) shouldBe Right("Order successful")
      }
    }
  }
}
