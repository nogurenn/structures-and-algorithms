import org.scalatest.funsuite.AnyFunSuite
import Searching._

class SearchingSuite extends AnyFunSuite with Utils {

  private val numbers = generateRandArrayInt()
  private val sortedNums = numbers.sorted

  test("linearSearch") {
    // returns Some(indexOf(x)) if x is in array
    assert(linearSearch(numbers, numbers.last).isDefined)
    // since `numbers` consists of nonnegatives, just find a negative number
    assert(linearSearch(numbers, -1).isEmpty)
  }

  test("linearSearchFunctional") {
    assert(linearSearch(numbers, numbers.last).isDefined)
    // since `numbers` consists of nonnegatives, just find a negative number
    assert(linearSearch(numbers, -1).isEmpty)
  }
}