import java.time.LocalDate
import scala.util.Random

case class Cafe(name: String, private val menu: Menu) {
  var employees: List[Person] = List(Person(s"John '$name''", 35, Some(jobFactory)))

  private var revenue: Map[Transaction, Double] = Map(
    Card -> 0,
    Cash -> 0,
  )

  private var sales: Map[ItemCategory, Double] = Map()

  private var discountsUsed: Map[String, Int] = Map()

  def getShoppingList: Map[String, Int] = {
    menu.getItemNamesWithStock.filter({
      case (_, quantity) => quantity == 0
    }).map({
      case (itemName, _) =>
        val item: Option[Item] = menu.getItemByName(itemName)
        itemName -> item.get.stock
    })
  }

  def useDiscount(discountType: String): Unit = {
    discountsUsed = discountsUsed + (discountType -> (discountsUsed.getOrElse(discountType, 0) + 1))
  }

  def getTotalDiscountsUsed: Map[String, Int] = discountsUsed

  private def addToSales(items: Map[Item, Int]): Unit = {
    items.foreach({
      case (item, quantity) =>
        sales = sales + (item.category -> (sales.getOrElse(item.category, 0.0) + item.price * quantity))
    })
  }

  def getTotalSales: Map[ItemCategory, Double] = sales

  private def addRevenue(bill: Bill): Unit =
    revenue = revenue + (bill.transactionType -> (revenue(bill.transactionType) + bill.total))

  def getRevenue: Map[Transaction, Double] = revenue

  def getTotalProfit: Double =
    revenue.values.sum - menu.items.map(item => item.price * item.stock).sum


  def showMenu: String = menu.toString

  def addEmployee(employee: Person): Unit = {
    employees = employees :+ employee
  }

  def takeOrder(
    customer: Person,
    items: Map[String, Int],
    transactionType: Transaction,
    toCurrencyCode: String = "GBP",
    addServiceCharge: Boolean = true)
    (customServiceCharge: Option[Double] = None): Either[Cafe.CafeError, Bill] = {
    val employee = employees(Random.nextInt(employees.size))

    val bill: Either[Cafe.CafeError, Bill] = placeOrder(customer, employee, items, transactionType, toCurrencyCode, addServiceCharge)(customServiceCharge)

    bill match {
      case Left(_) => ()
      case Right(b) =>
        addRevenue(b)
        addToSales(b.items)
    }

    bill
  }

  def placeOrder(
    customer: Person,
    employee: Person,
    items: Map[String, Int],
    transactionType: Transaction,
    toCurrencyCode: String = "GBP",
    addServiceCharge: Boolean = true)
    (customServiceCharge: Option[Double] = None): Either[Cafe.CafeError, Bill] = {
    items match {
      case i if i.isEmpty => Left(Cafe.OrderInvalidItemList("Order cannot be empty"))
      case _ =>
        menu.updateMenuAfterOrder(items) match {
          case Left(e) => Left(e)
          case Right(_) =>
            val itemsWithStock: Map[Item, Int] = items.map({
              case (itemName, quantity) => (menu.getItemByName(itemName).get, quantity)
            })
            val serviceCharge: Either[Cafe.OrderInvalidServiceCharge, Double] = if (addServiceCharge) {
              customServiceCharge match {
                case Some(charge) if charge >= 0 => Right(charge)
                case Some(_) => Left(Cafe.OrderInvalidServiceCharge("Service charge cannot be negative"))
                case None =>
                  itemsWithStock.keys match {
                    case ik if ik.exists(_.category == PremiumMeal) => Right(0.25)
                    case ik if ik.exists(_.category == HotFood) => Right(0.2)
                    case ik if ik.exists(_.category == ColdFood) => Right(0.1)
                    case _ => Right(0)
                  }
              }
            } else Right(0)

            serviceCharge.map(sc => Bill(this, customer, employee, itemsWithStock, sc, transactionType, toCurrencyCode))
        }
    }
  }

  def jobFactory: Cafe.CafeJob = {
    Cafe.CafeJob(LocalDate.now(), this)
  }

  def jobFactory(joinedDate: LocalDate): Cafe.CafeJob = {
    Cafe.CafeJob(joinedDate, this)
  }
}

object Cafe extends App {
  abstract class CafeError(message: String) extends Exception(message)

  case class MenuUnavailableItemError(message: String) extends CafeError(message)

  case class MenuInvalidItemError(message: String) extends CafeError(message)

  case class MenuInvalidQuantityError(message: String) extends CafeError(message)

  case class OrderInvalidServiceCharge(message: String) extends CafeError(message)

  case class OrderInvalidItemList(message: String) extends CafeError(message)

  case class CafeJob private(
    joinedDate: LocalDate,
    cafe: Cafe
  ) extends Job

  object CafeJob {
    private[Cafe] def apply(joinedDate: LocalDate, cafe: Cafe): CafeJob = {
      new CafeJob(joinedDate, cafe)
    }
  }

  val cafe: Cafe = Cafe("Default Cafe", Menu(List(
    Item("Coffee", 2.5, ColdDrink, 10),
    Item("Tea", 1.5, HotDrink, 20),
    Item("Sandwich", 3.0, ColdFood, 15),
    Item("Burger", 5.0, HotFood, 5),
    Item("Pasta", 4.0, PremiumMeal, 8)
  )))

  println(cafe.takeOrder(
    Person("Alice", 30, Some(cafe.jobFactory(LocalDate.now().minusMonths(6)))),
    Map("Coffee" -> 2, "Sandwich" -> 1, "Burger" -> 5),
    Card
  )(Some(0.15)))

  println()
  println(cafe.getRevenue)
  println(cafe.getTotalSales)
  println(cafe.getTotalDiscountsUsed)
  println(cafe.getTotalProfit)
  println(cafe.getShoppingList)
}