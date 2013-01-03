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

    case class Meta(legal:Boolean, votes:Int = 0) {
      override def toString = {
        if (!legal) "."
        else if (votes > 9) "*"
        else votes.toString
      }
    }

    val meta = BoardState { (x:Int, y:Int) =>
      val valid = try {
        board.play(color, x, y)
        true
      } catch {
        case _ => false
      }
      if (valid) {
        val votes = strategies.count(_.rate(color, x, y, board))
        new Meta(true, votes)
      } else {
        new Meta(false)
      }
    }

    println("meta: \n%s".format(meta))

    println(new Box {
      def children = Seq(
        Child(0, 0, new Str(board.toString)),
        Child(20, 0, new Str(meta.toString))
      )
    })


    val (x, y, _) = meta.filter(_._3.legal).maxBy(_._3.votes)
    (x, y)
  }
}