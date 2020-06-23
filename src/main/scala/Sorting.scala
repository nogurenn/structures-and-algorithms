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

  // https://stackoverflow.com/a/18847665/1983337
  // very interesting. copy pasta for reference
  def insertionSortFunctional[T](a: List[T])(implicit ord: Ordering[T]): List[T] = {
    def insert(x: T, xs: List[T]): List[T] = {
      val (l, r) = xs.span(ord.lt(_, x))
      l ::: (x :: r)
    }

    a.foldLeft(List.empty[T]) {
      case (acc, elem) => insert(elem, acc)
    }
  }
}