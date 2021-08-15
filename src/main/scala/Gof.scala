import scala.collection.mutable.ArrayBuffer

class Gof(rows: Int, cols: Int) {

  // Init double dim array filled with 0
  private val grid = Array.fill(rows,cols)(0)

  def setSeedState(aliveCoordinates: Array[Tuple2[Int,Int]]): Unit = {
    aliveCoordinates.foreach {
      case (x,y) => grid(x)(y) = 1
    }
  }

  def computeNextNthState(n: Int = 1): Unit = {
    (1 to n).foreach { run =>
      val toBeInverted = new ArrayBuffer[Tuple2[Int,Int]]()
      // to find neighbour co-ordinates scanning only the internal matrix, ignoring the border coordinates
      // also with the below approach it doesn't matter if iterating in reverse order
      (rows-2 to 1 by -1).foreach{i =>
        (cols-2 to 1 by -1).foreach{ j =>
          val (alive,dead) = findAliveAndDeadCells(i,j)
          if(grid(i)(j)==1 && alive<2) {
            toBeInverted += ((i,j))
          } else if(grid(i)(j)==1 && alive>3) {
            toBeInverted += ((i,j))
          } else if(grid(i)(j)==0 && alive==3) {
            toBeInverted += ((i,j))
          }
        }
      }
      toBeInverted.foreach { case (x,y) =>
        val newVal = if(grid(x)(y) == 1) 0 else 1
        grid(x)(y) = newVal
      }
    }
  }

  /**
    * @param c Coordinate for which neighbours need to be calculated
    * @return A tuple containing alive and dead neighbours respectively
    */
  def findAliveAndDeadCells(c: Tuple2[Int,Int]): Tuple2[Int,Int] = {
    val neighbors = Array[Tuple2[Int, Int]](
      (c._1 - 1, c._2 - 1),
      (c._1 - 1, c._2),
      (c._1 - 1, c._2 + 1),
      (c._1, c._2 - 1),
      (c._1, c._2 + 1),
      (c._1 + 1, c._2 - 1),
      (c._1 + 1, c._2),
      (c._1 + 1, c._2 + 1)
    )
    val alive = neighbors.count { case (x,y) => grid(x)(y) == 1 }
    val dead = 8 - alive
    (alive,dead)
  }

  def getGrid: Array[Array[Int]] = {
    println("==")
    grid.foreach(arr => println(arr.mkString(",")))
    grid
  }

}