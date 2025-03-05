import scala.annotation.tailrec

case class Menu private(items: List[Item]) {
  override def toString: String = items.mkString("\n")

  private var itemsWithStock: Map[Item, Int] = items.map(item => (item, item.stock)).toMap

  def isItemAvailable(item: Item, quantity: Int): Boolean =
    itemsWithStock.get(item).exists(stock => stock > 0 && stock >= quantity)

  def updateMenuItem(item: Item, stock: Int): Either[Cafe.CafeError, Map[Item, Int]] = {
    itemsWithStock.get(item) match {
      case None => Left(Cafe.MenuInvalidItemError(s"${item.name} not found in menu"))
      case Some(_) if stock >= 0 => Right(itemsWithStock + (item -> stock))
      case Some(_) => Left(Cafe.MenuInvalidQuantityError(s"Stock cannot be negative"))
    }
  }

  def updateMenuAfterOrder(items: Map[Item, Int]): Either[Cafe.CafeError, Map[Item, Int]] = {
    @tailrec
    def iterate(iItems: Map[Item, Int], acc: Map[Item, Int]): Either[Cafe.CafeError, Map[Item, Int]] = {
      iItems.headOption match {
        case None =>
          itemsWithStock = itemsWithStock ++ acc
          Right(itemsWithStock)
        case Some((item, quantity)) =>
          if (isItemAvailable(item, quantity)) {
            iterate(iItems.tail, acc + (item -> (itemsWithStock(item) - quantity)))
          } else {
            Left(Cafe.MenuUnavailableItemError(s"$item not available"))
          }
      }
    }

    items match {
      case i if i.isEmpty => Left(Cafe.OrderInvalidItemList("Item map cannot be empty"))
      case _ => iterate(items, Map())
    }
  }
}
