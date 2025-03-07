import java.time.LocalDate

sealed trait Job {
  val joinedDate: LocalDate
}

case class CafeJob(
  joinedDate: LocalDate,
  cafe: Cafe
) extends Job
