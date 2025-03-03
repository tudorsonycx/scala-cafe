case class Menu private(items: List[Item]) {
  override def toString: String = items.mkString("\n")
}