package kb

import util.Random

object Othello {

  def test {
    game(Board(), White)
  }

  def game(board:Board, color:Color, passed:Boolean = false) {

    val pass = board.score.empty == 0 || ! board.exists {
      case (x, y, _) => board.test(color, x, y)
    }

    val (result, next) = if (pass && ! passed) {
      (new Str("no legal play"), Some((board, color.opponent, true)))
    } else if (! pass) {
      val (result, nextBoard) = play(color, board)
      (result, Some((nextBoard, color.opponent, false)))
    }
    else {
      (new Str("%s wins".format(board.score.winner.getOrElse("(tie)"))), None)
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
        if (board.test(color, x, y)) {
        val strategies = color match {
        case White => Seq(new Strategy.Corner, new Strategy.Edge)
          case Black => Seq()
        }
        val votes = strategies.count(_.rate(color, x, y, board))
        new Meta(true, votes)
      } else {
        new Meta(false)
      }
    }

    val legal = meta.filter(_._3.legal)
    val (x, y, _) = rnd.shuffle(legal.filter(_._3.votes == legal.maxBy(_._3.votes)._3.votes)).head
    (new Str(meta.toString), board.play(color, x, y))
  }

  val rnd = new Random
}