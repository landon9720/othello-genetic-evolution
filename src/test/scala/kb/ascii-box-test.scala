package kb

import org.scalatest.FlatSpec

class BoxTest extends FlatSpec {
  "box" should "work" in {
    val box = new Box {
      val children = Seq(
        Child(0, 0, new Str("score: 33")),
        Child(1, 1, new Str("score: 343")),
        Child(1, 3, new Box {
          val children = Seq(
            Child(0, 0, new Str("12345678")),
            Child(1, 1, new Str("b2345678")),
            Child(2, 2, new Str("c2345678"))
          )
        })
      )
    }
    println(box.w)
    println(box.h)
    println(box.render)

//    val box2 = new Box {
//      val children = Seq(
//        (0, 0, new Str("***")),
//        (1, 1, box)
//      )
//    }
//    println(box2.render)

//    assert(box.render === """""")
  }
}
