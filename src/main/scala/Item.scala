sealed trait ItemCategory

sealed trait Food extends ItemCategory

sealed trait Drink extends ItemCategory

case object HotFood extends Food

case object ColdFood extends Food

case object PremiumMeal extends Food

case object HotDrink extends Drink

case object ColdDrink extends Drink

case object AlcoholicDrink extends Drink


case class Item(name: String, price: Double, category: ItemCategory, stock: Int = 0) {
  override def toString: String = s"$name ($category) - $price"
}