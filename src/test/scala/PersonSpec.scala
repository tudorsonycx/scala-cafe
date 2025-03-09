import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PersonSpec extends AnyWordSpec with Matchers {
  "Customer.applyForLoyaltyCard" should {
    "create a loyalty card when none exists and return a success message" when {
      "a drinks loyalty card is requested" in {
        val customer: Person = Person("John Doe", 30, job = None)
        customer.applyForLoyaltyCard("drinks") shouldBe Right("Drinks loyalty card created")
      }

      "a discount loyalty card is requested" in {
        val customer: Person = Person("John Doe", 30, job = None)
        customer.applyForLoyaltyCard("Discount") shouldBe Right("Discount loyalty card created")
      }
    }

    "return an error" when {
      "a loyalty card already exists" in {
        val customer: Person = Person("John Doe", 30, job = None, loyaltyCard = Some(DrinksLoyaltyCard()))
        customer.applyForLoyaltyCard("discount") shouldBe Left(Person.PersonErrorLoyaltyCardAlreadyExists)
      }

      "an invalid loyalty card type is requested" in {
        val customer: Person = Person("John Doe", 30, job = None)
        customer.applyForLoyaltyCard("invalid") shouldBe Left(Person.PersonErrorLoyaltyCardType("invalid is not a valid loyalty card type"))
      }

      "the person is under 18" in {
        val customer: Person = Person("John Doe", 17, job = None)
        customer.applyForLoyaltyCard("drinks") shouldBe Left(Person.PersonErrorPersonUnderAge)
      }
    }
  }

}
