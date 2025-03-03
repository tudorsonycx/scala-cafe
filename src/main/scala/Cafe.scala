import scala.annotation.tailrec

case class Cafe(name: String, private val menu: Menu) {
  def showMenu: String = menu.toString

  private val menuWithStock: Map[Item, Int] = menu.items.map(item => (item, item.stock)).toMap

  def getItemCost(item: Item, quantity: Int): Either[Exception, Double] =
    menuWithStock.get(item) match {
      case Some(stock) if stock >= quantity => Right(item.price * quantity)
      case Some(stock) => Left(
        Cafe.MenuInvalidQuantityError(s"Not enough stock for ${item.name}. Available: $stock"))
      case None => Left(Cafe.MenuInvalidItemError(s"${item.name} not found in menu"))
    }
}
object Cafe {
  case class MenuInvalidItemError(message: String) extends Exception(message)

  case class MenuInvalidQuantityError(message: String) extends Exception(message)
}