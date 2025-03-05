import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MenuSpec extends AnyWordSpec with Matchers {
  val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
  val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)
  val item2: Item = Item("Test Item 2", 20.0, HotFood, 5)
  val item3: Item = Item("Test Item 3", 30.0, PremiumMeal, 3)

  val itemInvalid: Item = Item("Invalid Item", 10.0, ColdFood)

  val menu: Menu = Menu(List(item0, item1, item2, item3))

  "Menu.toString" should {
    "return a string representation of the menu" in {

      menu.toString shouldBe "Test Item 0 (ColdDrink) - 5.0\nTest Item 1 (ColdFood) - 10.0\nTest Item 2 (HotFood) - 20.0\nTest Item 3 (PremiumMeal) - 30.0"
    }
  }

  "Menu.updateMenuItem" should {
    "return the updated menu" when {
      "called with valid item and stock" in {
        menu.updateMenuItem(item1, 10) shouldBe Right(Map(item0 -> 20, item1 -> 10, item2 -> 5, item3 -> 3))
      }
    }

    "return an error" when {
      "called with invalid item" in {
        menu.updateMenuItem(itemInvalid, 10) shouldBe Left(Cafe.MenuInvalidItemError("Invalid Item not found in menu"))
      }
      "called with negative stock" in {
        menu.updateMenuItem(item1, -1) shouldBe Left(Cafe.MenuInvalidQuantityError("Stock cannot be negative"))
      }
    }
  }
}
