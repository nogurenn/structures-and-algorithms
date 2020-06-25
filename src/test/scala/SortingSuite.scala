import org.scalatest.funsuite.AnyFunSuite
import Sorting._

class SortingSuite extends AnyFunSuite with Utils {
  private val numbers = generateRandArrayInt()
  private val sortedNums = numbers.sorted

  test("bubbleSort") {
    assert(sortedNums sameElements bubbleSort(numbers))
  }

  test("bubbleSortFunctional") {
    assert(Array(1,2,3,5,6) sameElements bubbleSortFunctional(List(5,3,1,6,2)))
    // DON'T DO IT at n = 50000
    // YOU HAVE BEEN WARNED
    // assert(sortedNums sameElements bubbleSortFunctional(numbers.toList))
  }

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
