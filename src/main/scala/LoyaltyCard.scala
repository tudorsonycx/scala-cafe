import java.time.LocalDate

trait LoyaltyCard {
  protected var timestamps: List[LocalDate] = List()
}

case class DrinksLoyaltyCard() extends LoyaltyCard {
  def addTimestamp(): Either[LoyaltyCard.LoyaltyCardError, String] = {
    if (timestamps.lastOption.contains(LocalDate.now())) {
      Left(LoyaltyCard.LoyaltyCardStampEarned)
    } else {
      timestamps = timestamps :+ LocalDate.now()
      Right("Loyalty card stamp added")
    }
  }

  def isNextFree: Boolean = {
    if (timestamps.length + 1 == 10) {
      timestamps = List()
      true
    } else {
      false
    }
  }
}

object LoyaltyCard {
  abstract class LoyaltyCardError(message: String) extends Exception(message)

  case object LoyaltyCardStampEarned extends LoyaltyCardError("Loyalty card stamp earned today already")
}