case class Bill(customer: Customer, items: Map[Item, Int], serviceCharge: Double) {
  def total: Double = {
    val itemTotal = items.map({
      case (item, quantity) => item.price * quantity
    }).sum
    itemTotal
  }

  override def toString: String = {
    val itemDetails = items.map({
      case (item, quantity) => s"${item.name} x $quantity"
    }).mkString("\n")
    s"Customer: ${customer.name}\nItems:\n$itemDetails\nSubtotal: $total\n" +
      s"Service Charge: $serviceCharge\nTotal: ${total * (1 + serviceCharge)}"
  }
}
