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
}
