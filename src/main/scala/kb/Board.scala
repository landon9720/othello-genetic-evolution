package kb

object Board {
	def apply() = {
		val state = new MutableBoardState[Color](collection.mutable.Seq.fill[Color](64)(Empty))
		state.set(3, 3, White)
		state.set(4, 4, White)
		state.set(3, 4, Black)
		state.set(4, 3, Black)
		new Board(state)
	}
}

class Board(val state:BoardState[Color]) extends Iterable[(Int, Int, Color)] {

  def test(color:Color, col:Int, row:Int):Boolean = try {
    play(color, col, row)
    true
  } catch {
    case _=> false
  }

	def play(color:Color, col:Int, row:Int):Board = {

		assert(state.get(col, row) == Empty, "not empty")

		val result = state.mutable

		result.set(col, row, color)

		val flipped = vectors.foldLeft(false) {
			(flipped:Boolean, dxdy:Tuple2[Int, Int]) =>
      val (dx, dy) = dxdy
			flip(result, color, col + dx, row + dy, dx, dy) || flipped
		}
		assert(flipped, "nothing flipped")

		new Board(result.immutable)
	}

	private val vectors = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

	private def flip(state:MutableBoardState[Color], color:Color, x:Int, y:Int, dx:Int, dy:Int):Boolean = {
    def in(x:Int, y:Int) = x >= 0 && x <= 7 && y >= 0 && y <= 7
    val x1 = x + dx
    val y1 = y + dy
    in(x, y) && (state.get(x, y) match {
      case Empty => false
      case c if in(x1, y1) && state.get(x1, y1) == color => {
        state.set(x, y, color)
        true
      }
      case c if c == color.opponent && flip(state, color, x + dx, y + dy, dx, dy) => {
        state.set(x, y, color)
        true
      }
      case _ => false
    })
	}

  def iterator = state.iterator

  case class Score(white:Int, black:Int, empty:Int) {
    def apply(color:Color) = color match {
      case White => white
      case Black => black
      case Empty => empty
    }
    override def toString = "W:%d B:%d E:%d".format(white, black, empty)
    val winner = if (white > black) Some(White) else if (black > white) Some(Black) else None
  }

  def score = {
    var w = 0
    var b = 0
    for ((_, _, c) <- this) {
      c match {
        case White => w = w + 1
        case Black => b = b + 1
        case _ =>
      }
    }
    Score(w, b, 64 - w -b)
  }
	
	override def toString = state.toString
}



