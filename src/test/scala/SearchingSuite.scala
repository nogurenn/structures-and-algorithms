import org.scalatest.funsuite.AnyFunSuite
import Searching._

import scala.util.Random

class SearchingSuite extends AnyFunSuite with Utils {

  private val numbers = generateRandArrayInt(5000000)
  private val sortedNums = numbers.sorted

  private val intArray = LazyList.continually(Random.between(-999, 1000)).take(50000).toArray
  private val kadaneSum = kadaneMaxSubArraySum(intArray)

  test("linearSearch") {
    // returns Some(indexOf(x)) if x is in array
    assert(linearSearch(numbers, numbers.last).isDefined)
    // since `numbers` consists of nonnegatives, just find a negative number
    assert(linearSearch(numbers, -1).isEmpty)
  }

  test("linearSearchFunctional") {
    assert(linearSearch(numbers, numbers.last).isDefined)
    assert(linearSearch(numbers, -1).isEmpty)
  }

  test("binarySearchIter") {
    assert(binarySearchIter(sortedNums, sortedNums.last).isDefined)
    assert(binarySearchIter(sortedNums, -1).isEmpty)
  }

  test("binarySearchRec") {
    assert(binarySearchRec(sortedNums, sortedNums.last).isDefined)
    assert(binarySearchRec(sortedNums, -1).isEmpty)
  }

  test("binarySearchFunctional") {
    assert(binarySearchFunctional(sortedNums.toList, sortedNums.last).isDefined)
    assert(binarySearchFunctional(sortedNums.toList, -1).isEmpty)
  }

  test("maxSubArraySum") {
    // from the book
    val arr = Array(13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7)
    assert((7, 10, 43) == maxSubArraySum(arr))
  }

  test("kadaneMaxSubArraySum") {
    // from the book
    val arr = Array(13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7)
    assert((7, 10, 43) == kadaneMaxSubArraySum(arr))
  }

  test("maxSubArraySum large input") {
    assert(kadaneSum == maxSubArraySum(intArray))
  }
}
