import scala.annotation.tailrec

case class Menu private(items: List[Item]) {
  override def toString: String = items.mkString("\n")

  private var itemsWithStock: Map[String, Int] = items.map(item => (item.name, item.stock)).toMap

  private val itemNamesWithItem: Map[String, Item] = items.map(item => (item.name, item)).toMap

  def getItemByName(itemName: String): Option[Item] = {
    itemNamesWithItem.get(itemName)
  }

  def isItemAvailable(itemName: String, quantity: Int): Boolean =
    itemsWithStock.get(itemName).exists(stock => stock > 0 && stock >= quantity)

  def updateMenuItem(itemName: String, stock: Int): Either[Cafe.CafeError, Map[String, Int]] = {
    itemsWithStock.get(itemName) match {
      case None => Left(Cafe.MenuInvalidItemError(s"$itemName not found in menu"))
      case Some(_) if stock >= 0 => Right(itemsWithStock + (itemName -> stock))
      case Some(_) => Left(Cafe.MenuInvalidQuantityError(s"Stock cannot be negative"))
    }
  }

  def updateMenuAfterOrder(items: Map[String, Int]): Either[Cafe.CafeError, String] = {
    @tailrec
    def iterate(iItems: Map[String, Int], acc: Map[String, Int]): Either[Cafe.CafeError, String] = {
      iItems.headOption match {
        case None =>
          itemsWithStock = itemsWithStock ++ acc
          Right("Order successful")
        case Some((itemName, quantity)) =>
          if (quantity <= 0) {
            Left(Cafe.MenuInvalidQuantityError(s"Order quantity cannot be negative"))
          } else if (isItemAvailable(itemName, quantity)) {
            iterate(iItems.tail, acc + (itemName -> (itemsWithStock(itemName) - quantity)))
          } else {
            Left(Cafe.MenuUnavailableItemError(s"$itemName not available"))
          }
      }
    }

    items match {
      case i if i.isEmpty => Left(Cafe.OrderInvalidItemList("Item map cannot be empty"))
      case _ => iterate(items, Map())
    }
  }
}
