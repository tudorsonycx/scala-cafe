case class Menu private(items: List[Item]) {
  override def toString: String = items.mkString("\n")

  private var itemsWithStock: Map[Item, Int] = items.map(item => (item, item.stock)).toMap

  def getItemCost(item: Item, quantity: Int): Either[Cafe.CafeError, Double] = {
    quantity match {
      case q if q <= 0 => Left(Cafe.MenuInvalidQuantityError(s"Quantity cannot be negative or zero"))
      case _ => itemsWithStock.get(item) match {
        case None => Left(Cafe.MenuInvalidItemError(s"${item.name} not found in menu"))
        case Some(stock) if stock < quantity => Left(
          Cafe.MenuInvalidQuantityError(s"Not enough stock for ${item.name}. Available: $stock"))
        case Some(_) => Right(item.price * quantity)
      }
    }
  }

  def updateMenuItem(item: Item, stock: Int): Either[Cafe.CafeError, Map[Item, Int]] = {
    itemsWithStock.get(item) match {
      case None => Left(Cafe.MenuInvalidItemError(s"${item.name} not found in menu"))
      case Some(_) if stock >= 0 => Right(itemsWithStock + (item -> stock))
      case Some(_) => Left(Cafe.MenuInvalidQuantityError(s"Stock cannot be negative"))
    }
  }

}