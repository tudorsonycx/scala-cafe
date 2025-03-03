sealed trait ItemCategory

case object HotFood extends ItemCategory

case object ColdFood extends ItemCategory

case object HotDrink extends ItemCategory

case object ColdDrink extends ItemCategory

case object AlcoholicDrink extends ItemCategory

case object PremiumMeal extends ItemCategory

case class Item(name: String, price: Float, category: ItemCategory, stock: Int = 0)