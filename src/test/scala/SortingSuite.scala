import org.scalatest.funsuite.AnyFunSuite
import Sorting._

class SortingSuite extends AnyFunSuite {
  private val numbers = Array(5, 3, 1, 6, 2)
  private val sortedNums = Array(1, 2, 3, 5, 6)

  test("insertionSort") {
    assert(sortedNums sameElements insertionSort(numbers.clone))
  }

  test("insertionSortFunctional") {
    assert(sortedNums sameElements insertionSortFunctional(numbers.toList))
  }
}
