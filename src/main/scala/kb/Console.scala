package kb

object Console {

	val E = Empty
	val B = Black
	val W = White

	var b = Board()
	def play(c:Color, col:Int, row:Int) {
		try {
			b = b.play(c, col, row)
			println(b)
		} catch {
			case _ => println("Sorry")
		}
	}
  def w(col:Int, row:Int) = play(W, col, row)
  def b(col:Int, row:Int) = play(B, col, row)
}
