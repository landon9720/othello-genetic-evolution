package kb

object BoardState {
  def apply[T](f: (Int, Int) => T):BoardState[T] = {
    new BoardState(collection.mutable.Seq.tabulate(64) { i =>
      f(i % 8, i / 8)
    })
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
