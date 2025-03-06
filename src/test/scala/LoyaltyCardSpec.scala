import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class LoyaltyCardSpec extends AnyWordSpec with Matchers {
  "DrinksLoyaltyCard.addTimestamp" should {
    "return Left if the last timestamp is today" in {
      val card: DrinksLoyaltyCard = DrinksLoyaltyCard()
      card.addTimestamp()
      card.addTimestamp() shouldBe Left(LoyaltyCard.LoyaltyCardStampEarned)
    }

    "return Right if the last timestamp is not today" in {
      val card: DrinksLoyaltyCard = DrinksLoyaltyCard()

      card.addTimestamp() shouldBe Right("Loyalty card stamp added")
    }
  }
}
