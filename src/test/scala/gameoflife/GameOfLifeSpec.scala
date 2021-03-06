package gameoflife

import org.scalatest.{FlatSpec, Matchers}

class GameOfLifeSpec extends FlatSpec with Matchers {

  val EMPTY_WORLD = "Empty world"

  val printer: List[Cell] => String = {
    case Nil => EMPTY_WORLD
    case world =>
      var sb = new StringBuilder
      val Cell(maxX, maxY) = world.foldLeft(world(0)) { case (Cell(x1, y1), Cell(x2, y2)) => Cell(math.max(x1, x2), math.max(y1, y2))}
      (0 to maxX).foreach(x => {
        (0 to maxY).foreach(y => {
          sb ++= (if (world contains Cell(x, y)) "X" else ".")
        })
        sb ++= "\n"
      })
      sb.toString()
  }

  val emptyPrinter: List[Cell] => String = {
    case _ => ""
  }

  it should "count zero neighbours" in {
    val gol = new GameOfLife(List(), emptyPrinter)

    gol.countNeighbours(10, 10) should be === 0
  }

  it should "count zero neighbours ignoring the given cell" in {
    val gol = new GameOfLife(List(Cell(0, 0)), emptyPrinter)

    gol.countNeighbours(0, 0) should be === 0
  }

  it should "count one neighbours" in {
    val gol = new GameOfLife(List(Cell(0, 0)), emptyPrinter)

    gol.countNeighbours(1, 1) should be === 1
  }

  it should "count two neighbours" in {
    val gol = new GameOfLife(List(Cell(1, 0), Cell(0, 1)), emptyPrinter)

    gol.countNeighbours(1, 1) should be === 2
  }

  it should "count three neighbours" in {
    val gol = new GameOfLife(List(Cell(1, 0), Cell(1, 1), Cell(0, 1)), emptyPrinter)

    gol.countNeighbours(0, 0) should be === 3
  }

  it should "count four neighbours" in {
    val gol = new GameOfLife(List(Cell(0, 0), Cell(1, 0), Cell(0, 1), Cell(2, 2)), emptyPrinter)

    gol.countNeighbours(1, 1) should be === 4
  }

  it should "iterate for an empty world" in {
    val gol = new GameOfLife(List(), printer)

    gol.noMoreLifeOnThePlanet() should be === true
    gol.printWorld() should be === EMPTY_WORLD
    gol.iterate()
    gol.noMoreLifeOnThePlanet() should be === true
    gol.printWorld() should be === EMPTY_WORLD
  }

  it should "observe a block pattern" in {
    val gol = new GameOfLife(List(Cell(1, 1), Cell(1, 2), Cell(2, 1), Cell(2, 2)), printer)
    val expected =
      """...
        |.XX
        |.XX
        |""".stripMargin

    gol.printWorld() should be === expected
    gol.iterate()
    gol.printWorld() should be === expected
  }

  it should "observe a blinker pattern" in {
    val gol = new GameOfLife(List(Cell(1, 1), Cell(1, 2), Cell(1, 3)), printer)
    val expectedAfterEvenIteration =
      """....
        |.XXX
        |""".stripMargin
    val expectedAfterOddIteration =
      """..X
        |..X
        |..X
        |""".stripMargin

    gol.printWorld() should be === expectedAfterEvenIteration
    gol.iterate()
    gol.printWorld() should be === expectedAfterOddIteration
    gol.iterate()
    gol.printWorld() should be === expectedAfterEvenIteration
    gol.iterate()
    gol.printWorld() should be === expectedAfterOddIteration
  }

  it should "observe a beacon period pattern" in {
    val gol = new GameOfLife(List(Cell(0, 0), Cell(1, 0), Cell(0, 1), Cell(1, 1), Cell(2, 2), Cell(3, 2), Cell(2, 3), Cell(3, 3)), printer)
    val expectedAfterEvenIteration =
      """XX..
        |XX..
        |..XX
        |..XX
        |""".stripMargin
    val expectedAfterOddIteration =
      """XX..
        |X...
        |...X
        |..XX
        |""".stripMargin

    gol.printWorld() should be === expectedAfterEvenIteration
    gol.iterate()
    gol.printWorld() should be === expectedAfterOddIteration
    gol.iterate()
    gol.printWorld() should be === expectedAfterEvenIteration
    gol.iterate()
    gol.printWorld() should be === expectedAfterOddIteration
  }
}
