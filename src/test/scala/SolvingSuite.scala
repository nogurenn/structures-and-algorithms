import org.scalatest.funsuite.AnyFunSuite

import Solving._

class SolvingSuite extends AnyFunSuite with Utils {

  private val n = 100
  private val matrixA = Array.fill(n)(generateRandArrayInt(n, 100))
  private val matrixB = Array.fill(n)(generateRandArrayInt(n, 100))

  test("squareMatrixMultiply") {
    // (row, col)
    // matrix(row)(col)
    val a = Array(Array(5, 8), Array(3, 8))
    val b = Array(Array(3, 8), Array(8, 9))
    val product = Array(Array[Float](79, 112), Array[Float](73, 96))
    val output = squareMatrixMultiply(a, b)

    for (i <- product.indices) {
      assert(product(i) sameElements output(i))
    }
  }
}
