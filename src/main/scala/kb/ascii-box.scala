package kb

abstract class Box {

  case class Child(dx:Int, dy:Int, box:Box)
  def children:Seq[Child]

  import scala.Console._

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

  def project(map:Array[Array[(Char, Option[String])]], dx:Int, dy:Int) {
    for (Child(x, y, child) <- children)
      child.project(map, x + dx, y + dy)
  }

  def render = {
    val map = Array.fill[(Char, Option[String])](w, h)((' ', None))
    project(map, 0, 0)
    var buf = ""
    for (y <- 0 until h) {
      val boxes = for (x <- 0 until w) yield map(x)(y)
      var lastCode:Option[String] = None
      for ((char, code) <- boxes) {
        if (code != lastCode) {
          buf += RESET
          buf += code.getOrElse("")
          lastCode = code
        }
        buf += char
      }
      buf += "\n"
    }
    buf
  }

  override def toString = render
}

class Str(s:String, code:Option[String] = None) extends Box {

  private val lines = s.split("\n")

  override lazy val w = lines.map(_.length).max
  override lazy val h = lines.length
  val children = Nil

  override def project(map:Array[Array[(Char, Option[String])]], dx:Int, dy:Int) {
    for (y <- 0 until h) {
      val line = lines(y)
      for (x <- 0 until line.length)
        map(x + dx).update(y + dy, (line.charAt(x), code))
    }
  }
}
