package kb

import scala.Console._

abstract class Box {

  case class Child(dx:Int, dy:Int, box:Box)
  def children:Seq[Child]

  lazy val w:Int = {
    val child = (children.maxBy { child =>
      child.dx + child.box.w
    })
    child.dx + child.box.w
  }

  lazy val h:Int = {
    val child = (children.maxBy { child =>
      child.dy + child.box.h
    })
    child.dy + child.box.h
  }

  def project(canvas:Array[Array[Char]], dx:Int, dy:Int) {
    for (Child(x, y, child) <- children)
      child.project(canvas, x + dx, y + dy)
  }

  def render = {

    val canvas = Array.fill(w, h)(' ')
    project(canvas, 0, 0)

    var buf = ""
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        buf += canvas(x)(y)
      }
      buf += "\n"
    }

    buf
  }
}

class Str(s:String, codes:Option[String] = None) extends Box {
  override lazy val w = s.length
  override lazy val h = 1
  val children = Nil

  override def project(canvas: Array[Array[Char]], dx: Int, dy: Int) {
    for (x <- 0 until w)
      canvas(x + dx).update(dy, s.charAt(x))
  }
}
