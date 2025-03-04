import scala.annotation.tailrec

case class Cafe(name: String, private val menu: Menu) {
  def showMenu: String = menu.toString

  var menuWithStock: Map[Item, Int] = menu.items.map(item => (item, item.stock)).toMap

  def getItemCost(item: Item, quantity: Int): Either[Cafe.CafeError, Double] = {
    quantity match {
      case q if q <= 0 => Left(Cafe.MenuInvalidQuantityError(s"Quantity cannot be negative or zero"))
      case _ => menuWithStock.get(item) match {
        case None => Left(Cafe.MenuInvalidItemError(s"${item.name} not found in menu"))
        case Some(stock) if stock < quantity => Left(
          Cafe.MenuInvalidQuantityError(s"Not enough stock for ${item.name}. Available: $stock"))
        case Some(_) => Right(item.price * quantity)
      }
    }
  }

  def updateMenuItem(item: Item, stock: Int): Either[Cafe.CafeError, Map[Item, Int]] = {
    menuWithStock.get(item) match {
      case None => Left(Cafe.MenuInvalidItemError(s"${item.name} not found in menu"))
      case Some(_) if stock >= 0 => Right(menuWithStock + (item -> stock))
      case Some(_) => Left(Cafe.MenuInvalidQuantityError(s"Stock cannot be negative"))
    }
  }

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

    @tailrec
    def iterate(iItems: Map[Item, Int], acc: Double): Either[Cafe.CafeError, Double] = {
      iItems.headOption match {
        case None =>
          items.foreach({
            case (item, quantity) =>
              updateMenuItem(item, menuWithStock(item) - quantity).foreach(menuWithStock = _)
          })

          Right(acc).flatMap(acc => serviceCharge.map(sc => acc * (1 + sc)))
        case Some((item, quantity)) =>
          getItemCost(item, quantity) match {
            case Left(e) => Left(e)
            case Right(cost) => iterate(iItems.tail, acc + cost)
          }
      }
    }

    items match {
      case i if i.isEmpty => Left(Cafe.OrderInvalidItemList("Order cannot be empty"))
      case _ => iterate(items, 0).map(total => Bill(customer, total))
    }
  }
}

object Cafe {
  abstract class CafeError(message: String) extends Exception(message)

  case class MenuInvalidItemError(message: String) extends CafeError(message)

  case class MenuInvalidQuantityError(message: String) extends CafeError(message)

  case class OrderInvalidServiceCharge(message: String) extends CafeError(message)

  case class OrderInvalidItemList(message: String) extends CafeError(message)
}