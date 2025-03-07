import java.time.LocalDate

trait LoyaltyCard {
  protected var timestamps: List[LocalDate] = List()

  def getTimestampsLength: Int = {
    timestamps.length
  }
}

case class DrinksLoyaltyCard() extends LoyaltyCard {
  def addTimestamp(): Either[LoyaltyCard.LoyaltyCardError, String] = {
    if (timestamps.lastOption.contains(LocalDate.now())) {
      Left(LoyaltyCard.LoyaltyCardStampEarned)
    } else {
      if (isNextFree) {
        timestamps = List()
      } else {
        timestamps = timestamps :+ LocalDate.now()
      }
      Right("Loyalty card stamp added")
    }
  }

  def isNextFree: Boolean = {
    if (timestamps.length + 1 == 10) {
      true
    } else {
      false
    }
  }
}

case class DiscountLoyaltyCard() extends LoyaltyCard {
  def addStar(): Either[LoyaltyCard.LoyaltyCardError, String] = {
    if (timestamps.lastOption.contains(LocalDate.now())) {
      Left(LoyaltyCard.LoyaltyCardStampEarned)
    } else if (getTimestampsLength < 8) {
      timestamps = timestamps :+ LocalDate.now()
      Right("You have earned a star")
    } else {
      Right("You have already earned 8 stars")
    }
  }

  def getStarCount: Int = {
    timestamps.length
  }
}

object LoyaltyCard {
  abstract class LoyaltyCardError(message: String) extends Exception(message)

  case object LoyaltyCardStampEarned extends LoyaltyCardError("Loyalty card stamp earned today already")
}