import java.time.LocalDate
import scala.util.Random

case class Cafe(name: String, private val menu: Menu) {
  var employees: List[Person] = List(Person(s"John '$name''", 35, Some(jobFactory)))

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

    placeOrder(customer, employee, items, transactionType, toCurrencyCode, addServiceCharge)(customServiceCharge)
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

object Cafe {
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
}