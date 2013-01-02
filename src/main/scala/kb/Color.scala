package kb

trait Color {
  def opponent:Color
}
object White extends Color {
  override def toString = "W"
  val opponent = Black
}
object Black extends Color {
  override def toString = "B"
  val opponent = White
}
object Empty extends Color {
  override def toString = "."
  def opponent = sys.error("")
}