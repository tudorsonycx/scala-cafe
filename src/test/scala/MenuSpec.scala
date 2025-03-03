import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MenuSpec extends AnyWordSpec with Matchers {
  "Menu.toString" should {
    "return a string representation of the menu" in {
      val item1: Item = Item("Pizza", 10.0f, HotFood)
      val item2: Item = Item("Coke", 2.0f, ColdDrink)

      val menu: Menu = Menu(List(item1, item2))

      menu.toString shouldBe "Pizza (HotFood) - 10.0\nCoke (ColdDrink) - 2.0"
    }
  }
}
