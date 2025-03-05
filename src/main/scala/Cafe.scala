case class Cafe(name: String, private val menu: Menu) {
  def showMenu: String = menu.toString

  def placeOrder(
    customer: Customer,
    items: Map[Item, Int],
    addServiceCharge: Boolean = true)
    (customServiceCharge: Option[Double] = None): Either[Cafe.CafeError, Bill] = {
    val serviceCharge: Either[Cafe.OrderInvalidServiceCharge, Double] = if (addServiceCharge) {
      customServiceCharge match {
        case Some(charge) if charge >= 0 => Right(charge)
        case Some(_) => Left(Cafe.OrderInvalidServiceCharge("Service charge cannot be negative"))
        case None =>
          items.keys match {
            case ik if ik.exists(_.category == PremiumMeal) => Right(0.25)
            case ik if ik.exists(_.category == HotFood) => Right(0.2)
            case ik if ik.exists(_.category == ColdFood) => Right(0.1)
            case _ => Right(0)
          }
      }
    } else Right(0)

    items match {
      case i if i.isEmpty => Left(Cafe.OrderInvalidItemList("Order cannot be empty"))
      case _ =>
        menu.updateMenuAfterOrder(items).flatMap(_ => serviceCharge.map(sc => Bill(customer, items, sc)))
    }
  }
}

object Cafe extends App {
  abstract class CafeError(message: String) extends Exception(message)

  case class MenuUnavailableItemError(message: String) extends CafeError(message)

  case class MenuInvalidItemError(message: String) extends CafeError(message)

  case class MenuInvalidQuantityError(message: String) extends CafeError(message)

  case class OrderInvalidServiceCharge(message: String) extends CafeError(message)

  case class OrderInvalidItemList(message: String) extends CafeError(message)
}
