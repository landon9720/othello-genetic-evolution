package kb

object Othello {

  def test {
    game(Board(), White)
  }

  def game(board:Board, color:Color, passed:Boolean = false) {

    val pass = board.score.empty == 0 || ! board.exists {
      case (x, y, Empty) => try {
        board.play(color, x, y)
        true
      } catch {
        case _ => false
      }
      case _ => false
    }

    val (result, next) = if (pass && ! passed) {
      (new Str("no legal play"), Some((board, color.opponent, true)))
    } else if (! pass) {
      val (result, nextBoard) = play(color, board)
      (result, Some((nextBoard, color.opponent, false)))
    }
    else {
      (new Str("game over"), None)
    }

    println(new Box {
      def children = Seq(
        Child(0, 0, new Str(color.toString)),
        Child(0, 1, new Str(board.score.toString)),
        Child(0, 2, new Str(board.toString)),
        Child(19, 2, result)
      )
    })

    next.map(next => (game _).tupled(next))
  }

  def play(color:Color, board:Board) = {

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
        val votes = Seq(new Strategy.Corner, new Strategy.Edge).count(_.rate(color, x, y, board))
        new Meta(true, votes)
      } else {
        new Meta(false)
      }
    }

    val (x, y, _) = meta.filter(_._3.legal).maxBy(_._3.votes)

    (new Str(meta.toString), board.play(color, x, y))
  }
}