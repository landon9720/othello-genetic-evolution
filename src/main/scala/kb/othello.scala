package kb

import util.control.Breaks
import util.control.Exception.Try

trait Color
object E extends Color {
	override def toString = "."
}
object B extends Color {
	override def toString = "B"
}
object W extends Color {
	override def toString = "W"
}

object Board {
	def apply() = {
		val data:Array[Color] = Array.fill(64)(E)
		data.update(3 + 3 * 8, W)
		data.update(4 + 4 * 8, W)
		data.update(3 + 4 * 8, B)
		data.update(4 + 3 * 8, B)
		new Board(data)
	}
}

class Board(val data:Seq[Color]) {

	private def xy(data:Seq[Color], x:Int, y:Int) = data(x + y * 8)
	private def xy(data:collection.mutable.Seq[Color], x:Int, y:Int) = data(x + y * 8)
	private def xy(data:collection.mutable.Seq[Color], x:Int, y:Int, c:Color) = data.update(x + y * 8, c)

	def play(color:Color, col:Int, row:Int):Board = {

		assert(xy(data, col, row) == E, "not empty")

		val data1 = collection.mutable.Seq(data:_*)

		xy(data1, col, row, color)

		val v0 = playVector(data1, color, col, row, 0, -1)   // north
		val v1 = playVector(data1, color, col, row, 1, -1)	 // northeast
		val v2 = playVector(data1, color, col, row, 1, 0)    // east
		val v3 = playVector(data1, color, col, row, 1, 1)    // southeast
		val v4 = playVector(data1, color, col, row, 0, 1)    // south
		val v5 = playVector(data1, color, col, row, -1, 1)	 // southwest
		val v6 = playVector(data1, color, col, row, -1, 0)	 // west
		val v7 = playVector(data1, color, col, row, -1, -1)  // northwest

		assert(v0 || v1 || v2 || v3 || v4 || v5 || v6 || v7, "nothing flipped")
		new Board(data1)
	}

	private def playVector(data:collection.mutable.Seq[Color], color:Color, col:Int, row:Int, dx:Int, dy:Int):Boolean = {

		var valid = false
		
		var x:Int = col + dx
		var y:Int = row + dy
		
		var c = xy(data.toSeq, x, y)
		if (x >= 0 && x < 8 && y >= 0 && y < 8 && c != color) {
			val _b0 = new Breaks
			_b0.breakable {
				while (x >= 0 && x < 8 && y >= 0 && y < 8) {
					if (c == E) {
						_b0.break
					} else if (c == color) {
						valid = true
						while (x != col || y != row) {
							x -= dx
							y -= dy
							xy(data, x, y, color)
						}
						_b0.break
					}
					x += dx
					y += dy
					c = xy(data, x, y)
				}
			}
		}

		valid
	}
	
	override def toString:String = {
		(for {
			y <- 0 until 8
		} yield {
			(for {
				x <- 0 until 8
			} yield {
				xy(data, x, y)
			}).mkString("", " ", "\n")
		}).mkString("")
	}
}
