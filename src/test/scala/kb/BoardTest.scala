package kb

import org.scalatest.FlatSpec

class BoardTest extends FlatSpec {

  "board" should "play a game" in {
    var b = Board()
    assert(b.toString === """7 . . . . . . . .
                            |6 . . . . . . . .
                            |5 . . . . . . . .
                            |4 . . . B W . . .
                            |3 . . . W B . . .
                            |2 . . . . . . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 3, 5)

    assert(b.toString === """7 . . . . . . . .
                            |6 . . . . . . . .
                            |5 . . . W . . . .
                            |4 . . . W W . . .
                            |3 . . . W B . . .
                            |2 . . . . . . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 4, 5)
    assert(b.toString === """7 . . . . . . . .
                            |6 . . . . . . . .
                            |5 . . . W B . . .
                            |4 . . . W B . . .
                            |3 . . . W B . . .
                            |2 . . . . . . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 5, 3)
    assert(b.toString === """7 . . . . . . . .
                            |6 . . . . . . . .
                            |5 . . . W B . . .
                            |4 . . . W W . . .
                            |3 . . . W W W . .
                            |2 . . . . . . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 2, 5)
    assert(b.toString === """7 . . . . . . . .
                            |6 . . . . . . . .
                            |5 . . B B B . . .
                            |4 . . . W W . . .
                            |3 . . . W W W . .
                            |2 . . . . . . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 2, 6)
    assert(b.toString === """7 . . . . . . . .
                            |6 . . W . . . . .
                            |5 . . B W B . . .
                            |4 . . . W W . . .
                            |3 . . . W W W . .
                            |2 . . . . . . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 4, 2)
    assert(b.toString === """7 . . . . . . . .
                            |6 . . W . . . . .
                            |5 . . B W B . . .
                            |4 . . . W B . . .
                            |3 . . . W B W . .
                            |2 . . . . B . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 1, 5)
    assert(b.toString === """7 . . . . . . . .
                            |6 . . W . . . . .
                            |5 . W W W B . . .
                            |4 . . . W B . . .
                            |3 . . . W B W . .
                            |2 . . . . B . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 1, 7)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B . . . . .
                            |5 . W W B B . . .
                            |4 . . . W B . . .
                            |3 . . . W B W . .
                            |2 . . . . B . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 5, 5)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B . . . . .
                            |5 . W W W W W . .
                            |4 . . . W W . . .
                            |3 . . . W B W . .
                            |2 . . . . B . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 2, 3)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B . . . . .
                            |5 . W W W W W . .
                            |4 . . . W W . . .
                            |3 . . B B B W . .
                            |2 . . . . B . . .
                            |1 . . . . . . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 4, 1)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B . . . . .
                            |5 . W W W W W . .
                            |4 . . . W W . . .
                            |3 . . B B W W . .
                            |2 . . . . W . . .
                            |1 . . . . W . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 6, 2)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B . . . . .
                            |5 . W W B W W . .
                            |4 . . . W B . . .
                            |3 . . B B W B . .
                            |2 . . . . W . B .
                            |1 . . . . W . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 5, 4)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B . . . . .
                            |5 . W W B W W . .
                            |4 . . . W W W . .
                            |3 . . B B W B . .
                            |2 . . . . W . B .
                            |1 . . . . W . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 0, 5)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B . . . . .
                            |5 B B B B W W . .
                            |4 . . . W W W . .
                            |3 . . B B W B . .
                            |2 . . . . W . B .
                            |1 . . . . W . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 3, 6)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B W . . . .
                            |5 B B B W W W . .
                            |4 . . . W W W . .
                            |3 . . B B W B . .
                            |2 . . . . W . B .
                            |1 . . . . W . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 6, 5)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B W . . . .
                            |5 B B B B B B B .
                            |4 . . . W W W . .
                            |3 . . B B W B . .
                            |2 . . . . W . B .
                            |1 . . . . W . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 3, 2)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B W . . . .
                            |5 B B B B B B B .
                            |4 . . . W W W . .
                            |3 . . B W W B . .
                            |2 . . . W W . B .
                            |1 . . . . W . . .
                            |0 . . . . . . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 4, 0)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B W . . . .
                            |5 B B B B B B B .
                            |4 . . . W B W . .
                            |3 . . B W B B . .
                            |2 . . . W B . B .
                            |1 . . . . B . . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 5, 2)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B W . . . .
                            |5 B B B B B B B .
                            |4 . . . W B W . .
                            |3 . . B W W W . .
                            |2 . . . W W W B .
                            |1 . . . . B . . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 5, 1)
    assert(b.toString === """7 . B . . . . . .
                            |6 . . B W . . . .
                            |5 B B B B B B B .
                            |4 . . . W B B . .
                            |3 . . B W W B . .
                            |2 . . . W W B B .
                            |1 . . . . B B . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 1, 6)
    assert(b.toString === """7 . B . . . . . .
                            |6 . W W W . . . .
                            |5 B B W B B B B .
                            |4 . . . W B B . .
                            |3 . . B W W B . .
                            |2 . . . W W B B .
                            |1 . . . . B B . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 2, 1)
    assert(b.toString === """7 . B . . . . . .
                            |6 . W W W . . . .
                            |5 B B W B B B B .
                            |4 . . . W B B . .
                            |3 . . B W B B . .
                            |2 . . . B W B B .
                            |1 . . B . B B . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 6, 4)
    assert(b.toString === """7 . B . . . . . .
                            |6 . W W W . . . .
                            |5 B B W B B B B .
                            |4 . . . W W W W .
                            |3 . . B W B W . .
                            |2 . . . B W B B .
                            |1 . . B . B B . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 0, 7)
    assert(b.toString === """7 B B . . . . . .
                            |6 . B W W . . . .
                            |5 B B B B B B B .
                            |4 . . . B W W W .
                            |3 . . B W B W . .
                            |2 . . . B W B B .
                            |1 . . B . B B . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 7, 2)
    assert(b.toString === """7 B B . . . . . .
                            |6 . B W W . . . .
                            |5 B B B B B B B .
                            |4 . . . B W W W .
                            |3 . . B W B W . .
                            |2 . . . B W W W W
                            |1 . . B . B B . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 7, 3)
    assert(b.toString === """7 B B . . . . . .
                            |6 . B W W . . . .
                            |5 B B B B B B B .
                            |4 . . . B W W B .
                            |3 . . B W B W . B
                            |2 . . . B W W B W
                            |1 . . B . B B . .
                            |0 . . . . B . . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 5, 0)
    assert(b.toString === """7 B B . . . . . .
                            |6 . B W W . . . .
                            |5 B B B B B B B .
                            |4 . . . B W W B .
                            |3 . . B W B W . B
                            |2 . . . B W W B W
                            |1 . . B . B W . .
                            |0 . . . . B W . .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 6, 0)
    assert(b.toString === """7 B B . . . . . .
                            |6 . B W W . . . .
                            |5 B B B B B B B .
                            |4 . . . B W W B .
                            |3 . . B W B W . B
                            |2 . . . B W W B W
                            |1 . . B . B W . .
                            |0 . . . . B B B .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 3, 0)
    assert(b.toString === """7 B B . . . . . .
                            |6 . B W W . . . .
                            |5 B B B B B B B .
                            |4 . . . B W W B .
                            |3 . . B W B W . B
                            |2 . . . B W W B W
                            |1 . . B . W W . .
                            |0 . . . W B B B .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 2, 7)
    assert(b.toString === """7 B B B . . . . .
                            |6 . B B B . . . .
                            |5 B B B B B B B .
                            |4 . . . B W W B .
                            |3 . . B W B W . B
                            |2 . . . B W W B W
                            |1 . . B . W W . .
                            |0 . . . W B B B .
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 7, 0)
    assert(b.toString === """7 B B B . . . . .
                            |6 . B B B . . . .
                            |5 B B B B B B B .
                            |4 . . . B W W B .
                            |3 . . B W B W . B
                            |2 . . . B W W B W
                            |1 . . B . W W . .
                            |0 . . . W W W W W
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 6, 3)
    assert(b.toString === """7 B B B . . . . .
                            |6 . B B B . . . .
                            |5 B B B B B B B .
                            |4 . . . B W B B .
                            |3 . . B W B B B B
                            |2 . . . B W W B W
                            |1 . . B . W W . .
                            |0 . . . W W W W W
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 1, 3)
    assert(b.toString === """7 B B B . . . . .
                            |6 . B B B . . . .
                            |5 B B B B B B B .
                            |4 . . . B W B B .
                            |3 . W W W B B B B
                            |2 . . . B W W B W
                            |1 . . B . W W . .
                            |0 . . . W W W W W
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(Black, 7, 1)
    assert(b.toString === """7 B B B . . . . .
                            |6 . B B B . . . .
                            |5 B B B B B B B .
                            |4 . . . B W B B .
                            |3 . W W W B B B B
                            |2 . . . B W W B B
                            |1 . . B . W W . B
                            |0 . . . W W W W W
                            |  0 1 2 3 4 5 6 7""".stripMargin)

    b = b.play(White, 3, 7)
    assert(b.toString === """7 B B B W . . . .
                            |6 . B B W . . . .
                            |5 B B B W B B B .
                            |4 . . . W W B B .
                            |3 . W W W B B B B
                            |2 . . . B W W B B
                            |1 . . B . W W . B
                            |0 . . . W W W W W
                            |  0 1 2 3 4 5 6 7""".stripMargin)


  }
}
