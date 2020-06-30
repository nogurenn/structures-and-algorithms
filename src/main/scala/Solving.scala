object Solving {
  // rename to Mathing? M A T H I N G

  type Matrix[T] = Array[Array[T]]

  def squareMatrixMultiply(a: Matrix[Int], b: Matrix[Int]): Matrix[Float] = {
    // n x n matrix

    val n = a.length
    val product: Matrix[Float] = Array.ofDim[Float](n, n)

    for {
      i <- 0 until n
      j <- 0 until n
      k <- 0 until n
    } {
      product(i)(j) += a(i)(k) * b(k)(j)
    }

    product
  }
}
