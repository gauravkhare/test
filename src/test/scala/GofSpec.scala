import org.scalatest.funsuite.AnyFunSuite

class GofSpec extends AnyFunSuite {

  test("Scenario: Compute next state of grid") {

    val ob = new Gof(5,7)
    ob.setSeedState(Array((1,3),(2,4),(3,2),(3,3),(3,4)))
    assert(isEqual(ob.getGrid,Array(
      Array(0,0,0,0,0,0,0),
      Array(0,0,0,1,0,0,0),
      Array(0,0,0,0,1,0,0),
      Array(0,0,1,1,1,0,0),
      Array(0,0,0,0,0,0,0)
    )),"Seed state is incorrectly set")
    ob.computeNextNthState()
    assert(isEqual(ob.getGrid,Array(
      Array(0,0,0,0,0,0,0),
      Array(0,0,0,0,0,0,0),
      Array(0,0,1,0,1,0,0),
      Array(0,0,0,1,1,0,0),
      Array(0,0,0,0,0,0,0)
    )),"Next state is computed incorrectly")
  }

  def isEqual(array1: Array[Array[Int]],array2: Array[Array[Int]]): Boolean = {
    if(array1.length != array2.length)
      return false

    for (i <- array1.indices) {
      if(!array1(i).sameElements(array2(i))) {
        return false
      }
    }
    true
  }

}
