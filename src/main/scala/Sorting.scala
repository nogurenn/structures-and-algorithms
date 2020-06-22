object Sorting {

  def insertionSort(a: Array[Int]): Array[Int] = {
    for (n <- 1 until a.length) {
      val elem = a(n)
      // compare elem against a(n-1) with backward steps
      var pos = n
      while (pos > 0 && a(pos - 1) > elem) {
        // if new pos not yet found, move the left-side element forward by 1
        a(pos) = a(pos - 1)
        pos = pos - 1
      }
      a(pos) = elem
    }
    a
  }

  def insertionSortFunctional[T](a: Seq[T])(implicit ord: Ordering[T]): Seq[T] = {
    def insert(x: T, xs: Seq[T]): Seq[T] =
      // insert element to the left of a number that's bigger than x
      if (xs.isEmpty || ord.lteq(x, xs.head)) x +: xs
      else xs.head +: insert(x, xs.tail)

    if (a.isEmpty) Nil
    else insert(a.head, insertionSortFunctional(a.tail))
  }
}