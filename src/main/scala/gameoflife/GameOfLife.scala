package gameoflife

case class Cell(x: Int, y: Int)

/**
 * GameOfLife implementation on a virtually auto expandable world (limits are the Int.min and Int.max)
 * @param seed Initial life
 * @param printFunction a function to print the world's status
 */
class GameOfLife(seed: List[Cell], printFunction: (List[Cell] => Any)) {
  def printWorld() = printFunction(world)

  private var world = seed

  def noMoreLifeOnThePlanet() = world.isEmpty

  def iterate() {
    world match {
      case Nil => // empty world, do nothing
      case h :: t =>
        val Cell(minX, minY) = t.foldLeft(h) { case (Cell(x1, y1), Cell(x2, y2)) => Cell(math.min(x1, x2), math.min(y1, y2))}
        val Cell(maxX, maxY) = t.foldLeft(h) { case (Cell(x1, y1), Cell(x2, y2)) => Cell(math.max(x1, x2), math.max(y1, y2))}
        world = (minX - 1 to maxX + 1).flatMap(x => {
          (minX - 1 to maxY + 1).flatMap(y => {
            if (world contains Cell(x, y)) {
              countNeighbours(x, y) match {
                case n if n < 2 || n > 3 => List() // underpopulation || overpopulated, this cell dies
                case n if n == 2 || n == 3 => List(Cell(x, y)) // this cell goes to next generation
              }
            } else {
              if (countNeighbours(x, y) == 3) {
                List(Cell(x, y)) //a new cell is created
              } else {
                List()
              }
            }
          })
        }).toList
    }
  }

  /**
   * Counts the neighbours, defined as number of alive cells in the 8 positions around this cell
   */
  def countNeighbours(x: Int, y: Int) = {
    List(x - 1, x, x + 1).flatMap(xC => List(y - 1, y, y + 1).map(yC => !(x == xC && y == yC) && world.contains(Cell(xC, yC)))).count(a => a)
  }
}
