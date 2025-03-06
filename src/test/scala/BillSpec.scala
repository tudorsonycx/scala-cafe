import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BillSpec extends AnyWordSpec with Matchers {
  "Bill.toString" should {
    "return a string representation of the bill" in {
      val item0: Item = Item("Test Item 0", 5.0, ColdDrink, 20)
      val item1: Item = Item("Test Item 1", 10.0, ColdFood, 5)
      val customer: Customer = Customer("Test Customer", 18)
      val items: Map[Item, Int] = Map(item0 -> 2, item1 -> 1)
      val serviceCharge: Double = 0.1
      val bill: Bill = Bill(customer, items, serviceCharge)

      val expectedResult =
        s"Customer: Test Customer\nItems:\nTest Item 0 x 2\nTest Item 1 x 1\nSubtotal: 20.0\nService Charge: 0.1\nTotal: 22.0"

      bill.toString shouldBe expectedResult
    }
  }
}
