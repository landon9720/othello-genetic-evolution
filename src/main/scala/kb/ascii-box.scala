package kb

import scala.Console._

trait Box {
  def put(line:Line)
}

trait Line {
  def width:Int
  override def toString:String
}

case class Str(s:String, codes:Option[String] = None) extends Line {
  val width = s.length
  override def toString = "%s%s%s".format(codes, s, RESET)
}