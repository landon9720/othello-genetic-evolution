package kb

object Othello {

  def test {
    game(Board(), White)
  }

  val strategies = Seq(
    new Strategy.Corner,
    new Strategy.Edge
  )

  private def game(board:Board, color:Color, passed:Boolean = false) {

    println("%s's move".format(color))
    println(board.score)

    val pass = board.score.empty == 0 || ! board.exists {
      case (x, y, Empty) => try {
        board.play(color, x, y)
        true
      } catch {
        case _ => false
      }
      case _ => false
    }

    if (pass && ! passed) {
      println("%s passed".format(color))
      game(board, color.opponent, true)
    } else if (! pass) {
      val (x, y) = color match {
        case White => play(White, board)
        case Black => play(Black, board)
      }
      game(board.play(color, x, y), color.opponent, false)
    } else {
      println("Game Over")
    }
  }

  private def play(color:Color, board:Board):(Int, Int) = {

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