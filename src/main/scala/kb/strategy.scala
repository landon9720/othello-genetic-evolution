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

class SuperStrategy(
  strategies:Iterable[Strategy] = Seq(
    new Strategy.Corner,
    new Strategy.Edge
  )
) {
  def play(color:Color, board:Board):(Int, Int) = {

    val legal = BoardState { (x:Int, y:Int) =>
        try {
          board.play(color, x, y)
          true
        } catch {
          case _ => false
        }
    }

    class Score(var votes:Int)
    val scores = new MutableBoardState[Score](collection.mutable.Seq.fill(64)(new Score(0)))
    for {
      s <- strategies
      (x, y) <- legal
      if s.rate(color, x, y, board)
    } {
      val score = scores.get(x, y)
      score.votes = score.votes + 1
    }

    val (x, y, _) = scores.maxBy(_._3.votes)
    (x, y)
  }
}