case class Bill(customer: Customer, items: Map[Item, Int], serviceCharge: Double) {
  private def subTotal: Double = {
    val itemTotal = items.map({
      case (item, quantity) => item.price * quantity
    }).sum
    itemTotal
  }

  def total: Double = subTotal * (1 + serviceCharge)

  override def toString: String = {
    val itemDetails = items.map({
      case (item, quantity) => s"${item.name} x $quantity"
    }).mkString("\n")
    s"Customer: ${customer.name}\nItems:\n$itemDetails\nSubtotal: $subTotal\n" +
      s"Service Charge: $serviceCharge\nTotal: $total"
  }
}
