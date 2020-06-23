import org.scalatest.funsuite.AnyFunSuite
import Sorting._

import scala.util.Random

class SortingSuite extends AnyFunSuite {
  // generate array of n length with random values [0, Int.MaxValue)
  private val n = 50000
  private val numbers = Array.fill(n)(Random.nextInt(Int.MaxValue))
  private val sortedNums = numbers.sorted

  test("insertionSort") {
    assert(sortedNums sameElements insertionSort(numbers))
  }

  test("insertionSortFunctional") {
    assert(sortedNums sameElements insertionSortFunctional(numbers.toList))
  }

  test("selectionSort") {
    assert(sortedNums sameElements selectionSort(numbers))
  }

  test("selectionSortFunctional") {
    assert(sortedNums sameElements selectionSortFunctional(numbers.toList))
  }

  test("mergeSort") {
    assert(sortedNums sameElements mergeSort(numbers))
  }

  test("mergeSortFunctional") {
    assert(sortedNums sameElements mergeSortFunctional(numbers.toList))
  }
}
