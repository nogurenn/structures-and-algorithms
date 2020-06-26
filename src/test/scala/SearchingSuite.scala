import org.scalatest.funsuite.AnyFunSuite
import Searching._

class SearchingSuite extends AnyFunSuite with Utils {

  private val numbers = generateRandArrayInt(5000000)
  private val sortedNums = numbers.sorted

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
}
