package kb

trait Strategy {
	def rate(c:Color, col:Int, row:Int, board:Board):Boolean
}

object Strategy {
	class Corner extends Strategy {
		def rate(c: Color, col: Int, row: Int, board: Board) =
			(col == 0 && row == 0) ||
			(col == 0 && row == 7) ||
			(col == 7 && row == 0) ||
			(col == 7 && row == 7)
	}
	class Edge extends Strategy {
		def rate(c: Color, col: Int, row: Int, board: Board) =
			((row == 0 || row == 7) && col >= 2 && col <= 5) ||
			((col == 0 || col == 7) && row >= 2 && row <= 5)
	}
}