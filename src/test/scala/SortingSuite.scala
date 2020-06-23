import org.scalatest.funsuite.AnyFunSuite
import Sorting._

import scala.util.Random

class SortingSuite extends AnyFunSuite {
  // generate array of n length with random values [0, 1000)
  private val n = 50000
  private val numbers = Array.fill(n)(Random.nextInt(1000))
  private val sortedNums = numbers.sorted

  test("insertionSort") {
    assert(sortedNums sameElements insertionSort(numbers.clone))
  }

  test("insertionSortFunctional") {
    assert(sortedNums sameElements insertionSortFunctional(numbers.toList))
  }
}
