package gameoflife

/**
 * Small application to run the blinker pattern on a 10 iterations base and 1 sec pause between iterations
 */
object GameOfLifeApp extends App {

  val printWorld: List[Cell] => Any = {
    case Nil => println("No more life on this planet...")
    case world =>
      val Cell(maxX, maxY) = world.foldLeft(world(0)) { case (Cell(x1, y1), Cell(x2, y2)) => Cell(math.max(x1, x2), math.max(y1, y2))}
      (0 to maxX).map(x => {
        (0 to maxY).map(y => {
          if (world contains Cell(x, y)) print("X") else print(" ")
        })
        println()
      })
      println()
  }

  val seed = List(Cell(1, 1), Cell(1, 2), Cell(2, 1), Cell(2, 2), Cell(3, 3), Cell(3, 4), Cell(4, 3), Cell(4, 4))

  val gol = new GameOfLife(seed, printWorld)

  println(s"Game starting:")
  gol.printWorld()

  (1 to 10).map(it => {
    Thread.sleep(1000)
    if (gol.noMoreLifeOnThePlanet()) {
      throw new RuntimeException("No more life on this planet")
    }
    println(s"Iteration: $it")
    gol.iterate()
    gol.printWorld()
  })
}
