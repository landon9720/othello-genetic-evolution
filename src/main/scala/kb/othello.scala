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

case class Score(white:Int, black:Int, empty:Int) {
  def apply(color:Color) = color match {
    case White => white
    case Black => black
    case Empty => empty
  }
  override def toString = "W:%d B:%d E:%d".format(white, black, empty)
}

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

class BoardState[T](seq:Seq[T]) extends Iterable[(Int, Int, T)] {

	def get(x:Int, y:Int) =
		seq(x + y * 8)

  def iterator = (
    for {
      x <- 0 until 8
      y <- 0 until 8
    } yield (x, y, get(x, y))
  ).iterator

  override def toString = (for (y <- 0 until 8 reverse) yield {
    (for (x <- 0 until 8) yield {
      get(x, y)
    }).mkString(y + " ", " ", "\n")
  }).mkString("", "", "  0 1 2 3 4 5 6 7")
		
	def mutable = new MutableBoardState(collection.mutable.Seq(seq:_*))
}

object BoardState {
  def apply[T](f: (Int, Int) => T):BoardState[T] = {
    new BoardState(collection.mutable.Seq.tabulate(64) { i =>
      f(i % 8, i / 8)
    })
  }
}

class MutableBoardState[T](seq:collection.mutable.Seq[T])
	extends BoardState[T](seq) {

	def set(x:Int, y:Int, t:T) {
		seq.update(x + y * 8, t)
	}
	
	def immutable = new BoardState(seq.toSeq) // toSeq to make immutable
}

class Board(val state:BoardState[Color]) extends Iterable[(Int, Int, Color)] {

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

	val vectors =
		Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

	def flip(state:MutableBoardState[Color], color:Color, x:Int, y:Int, dx:Int, dy:Int):Boolean = {
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