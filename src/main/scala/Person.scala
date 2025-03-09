case class Person(name: String, age: Int, var job: Option[Job], var loyaltyCard: Option[LoyaltyCard] = None) {
  def applyForLoyaltyCard(cardType: String): Either[Person.PersonError, String] = {
    if (!loyaltyCard.isDefined && age >= 18) {
      val (newCard: Option[LoyaltyCard], message: Either[Person.PersonError, String]) =
        cardType.toLowerCase match {
          case "drinks" =>
            (Some(DrinksLoyaltyCard()), Right("Drinks loyalty card created"))
          case "discount" =>
            (Some(DiscountLoyaltyCard()), Right("Discount loyalty card created"))
          case _ =>
            (None, Left(Person.PersonErrorLoyaltyCardType(s"$cardType is not a valid loyalty card type")))
        }

      loyaltyCard = newCard
      message
    } else if (age < 18) {
      Left(Person.PersonErrorPersonUnderAge)
    } else {
      Left(Person.PersonErrorLoyaltyCardAlreadyExists)
    }
  }
}

object Person extends {
  abstract class PersonError(message: String) extends Exception(message)

  case class PersonErrorLoyaltyCardType(message: String) extends PersonError(message)

  case object PersonErrorPersonUnderAge extends PersonError("Person is under 18 years old")

  case object PersonErrorLoyaltyCardAlreadyExists extends PersonError("Loyalty card already exists")
}