import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate
import scala.annotation.tailrec

class LoyaltyCardSpec extends AnyWordSpec with Matchers {
  final def getNTimestampsList(n: Int): List[LocalDate] = {
    @tailrec
    def iterate(i: Int = 0, acc: List[LocalDate] = List()): List[LocalDate] = {
      if (i == n) {
        acc
      } else {
        iterate(i + 1, acc :+ LocalDate.now().plusDays(n))
      }
    }

    iterate()
  }

  "DrinksLoyaltyCard.addTimestamp" should {
    "return Left if the last timestamp is today" in {
      val card: DrinksLoyaltyCard = DrinksLoyaltyCard()
      card.addTimestamp()
      card.addTimestamp() shouldBe Left(LoyaltyCard.LoyaltyCardStampEarned)
    }

    "return Right if the last timestamp is not today" in {
      val card: DrinksLoyaltyCard = DrinksLoyaltyCard()

      card.addTimestamp() shouldBe Right("Loyalty card stamp added")

      card.getTimestampsLength shouldBe 1
    }

    "return Right if the last timestamp is not today and reset the card" in {
      val timestampsMock: List[LocalDate] = getNTimestampsList(9)

      val card: DrinksLoyaltyCard = new DrinksLoyaltyCard() {
        timestamps = timestampsMock
      }

      card.addTimestamp() shouldBe Right("Loyalty card stamp added")

      card.getTimestampsLength shouldBe 0
    }
  }

  "DrinksLoyaltyCard.isNextFree" should {
    "return false if the card doesn't have 9 stamps" in {
      val card: DrinksLoyaltyCard = DrinksLoyaltyCard()

      card.isNextFree shouldBe false
    }

    "return true when the card has 9 stamps" in {

      val timestampsMock: List[LocalDate] = getNTimestampsList(9)

      val card: DrinksLoyaltyCard = new DrinksLoyaltyCard() {
        timestamps = timestampsMock
      }

      card.isNextFree shouldBe true
    }
  }

  "DiscountLoyaltyCard.addStar" should {
    "return Left if the last star was earned today" in {
      val card: DiscountLoyaltyCard = DiscountLoyaltyCard()

      card.addStar()

      card.addStar() shouldBe Left(LoyaltyCard.LoyaltyCardStampEarned)
    }

    "return Right if the last star was not earned today" when {
      "the card has less than 8 stars" in {
        val card: DiscountLoyaltyCard = DiscountLoyaltyCard()

        card.addStar() shouldBe Right("You have earned a star")

        card.getStarCount shouldBe 1
      }

      "the card has 8 stars" in {
        val timestampsMock: List[LocalDate] = getNTimestampsList(8)

        val card: DiscountLoyaltyCard = new DiscountLoyaltyCard() {
          timestamps = timestampsMock
        }

        card.addStar() shouldBe Right("You have already earned 8 stars")

        card.getStarCount shouldBe 8
      }
    }
  }
}
