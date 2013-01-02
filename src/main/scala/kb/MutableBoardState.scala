package kb

class MutableBoardState[T](seq:collection.mutable.Seq[T])
	extends BoardState[T](seq) {

	def set(x:Int, y:Int, t:T) {
		seq.update(x + y * 8, t)
	}

	def immutable = new BoardState(seq.toSeq) // toSeq to make immutable
}
