import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate
import scala.annotation.tailrec

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

  "DrinksLoyaltyCard.isNextFree" should {
    "return false if the card doesn't have 10 stamps" in {
      val card: DrinksLoyaltyCard = DrinksLoyaltyCard()

      card.isNextFree shouldBe false
    }

    "return true when the card has 10 stamps" in {
      @tailrec
      def iterate(n: Int = 0, acc: List[LocalDate] = List()): List[LocalDate] = {
        if (n == 10) {
          acc
        } else {
          iterate(n + 1, acc :+ LocalDate.now().plusDays(n))
        }
      }

      val timestampsMock: List[LocalDate] = iterate()

      val card: DrinksLoyaltyCard = new DrinksLoyaltyCard() {
        timestamps = timestampsMock
      }

      card.isNextFree shouldBe true
    }
  }
}
