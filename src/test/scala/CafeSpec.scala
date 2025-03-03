import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CafeSpec extends AnyWordSpec with Matchers {
  val item1: Item = Item("Test Item 1", 10.0, ColdFood)
  val item2: Item = Item("Test Item 2", 20.0, HotFood, 5)
  val item3: Item = Item("Test Item 3", 30.0, PremiumMeal, 3)

  val cafe: Cafe = Cafe("Test Cafe", Menu(List(item1, item2, item3)))

  "Cafe.showMenu" should {
    "return a string representation of the menu" in {
      val expectedResult: String =
        "Test Item 1 (ColdFood) - 10.0\nTest Item 2 (HotFood) - 20.0\nTest Item 3 (PremiumMeal) - 30.0"

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
        val result: Either[Exception, Double] = cafe.getItemCost(Item("Invalid Item", 10.0, ColdFood), 10)

        val expected: Left[Exception, Nothing] =
          Left(Cafe.MenuInvalidItemError("Invalid Item not found in menu"))

        result shouldBe expected
      }

      "called with invalid quantity" in {
        val result: Either[Exception, Double] = cafe.getItemCost(item1, 1)

        val expectedResult: Left[Exception, Nothing] =
          Left(Cafe.MenuInvalidQuantityError("Not enough stock for Test Item 1. Available: 0"))

        result shouldBe expectedResult
      }
    }
  }
}
