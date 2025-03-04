import scala.annotation.tailrec

case class Cafe(name: String, private val menu: Menu) {
  def showMenu: String = menu.toString

  var menuWithStock: Map[Item, Int] = menu.items.map(item => (item, item.stock)).toMap

  def getItemCost(item: Item, quantity: Int): Either[Cafe.CafeMenuError, Double] =
    menuWithStock.get(item) match {
      case Some(stock) if stock >= quantity => Right(item.price * quantity)
      case Some(stock) => Left(
        Cafe.MenuInvalidQuantityError(s"Not enough stock for ${item.name}. Available: $stock"))
      case None => Left(Cafe.MenuInvalidItemError(s"${item.name} not found in menu"))
    }

  def updateMenuItem(item: Item, stock: Int): Either[Cafe.CafeMenuError, Map[Item, Int]] = {
    menuWithStock.get(item) match {
      case None => Left(Cafe.MenuInvalidItemError(s"${item.name} not found in menu"))
      case Some(_) if stock >= 0 => Right(menuWithStock + (item -> stock))
      case Some(_) => Left(Cafe.MenuInvalidQuantityError(s"Stock cannot be negative"))
    }
  }
}

object Cafe {
  abstract class CafeMenuError(message: String) extends Exception(message)

  case class MenuInvalidItemError(message: String) extends CafeMenuError(message)

  case class MenuInvalidQuantityError(message: String) extends CafeMenuError(message)
}