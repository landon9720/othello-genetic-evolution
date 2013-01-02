package kb

class Game(
  white:SuperStrategy = new SuperStrategy,
  black:SuperStrategy = new SuperStrategy
) {
  def play(board:Board, color:Color, passed:Boolean = false) {

    println("%s's move".format(color))
    println(board)

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
      play(board, color.opponent, true)
    } else if (! pass) {
      val (x, y) = color match {
        case White => white.play(White, board)
        case Black => black.play(Black, board)
      }
      play(board.play(color, x, y), color.opponent, false)
    }
  }
}
