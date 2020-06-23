import scala.annotation.tailrec

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
  
  def selectionSort(a: Array[Int]): Array[Int] = {
    val n = a.length
    for (pos <- 0 until n-1) {
      var smallest = pos
      for (i <- pos+1 until n) {
        if (a(i) < a(smallest))
          smallest = i
      }
      // save first elem and perform swap
      val firstElem = a(pos)
      a(pos) = a(smallest)
      a(smallest) = firstElem
    }
    a
  }

  def selectionSortFunctional(a: List[Int]): List[Int] = {
    def findMaximum(xs: List[Int]): List[Int] = {
      xs.tail.foldLeft(List(xs.head)) { case (acc, elem) =>
        if (elem > acc.head)
          elem :: acc
        else
          acc.head :: elem :: acc.tail
      }
    }

    @tailrec
    def iter(xs: List[Int], acc: List[Int]): List[Int] = {
      if (xs.isEmpty) acc
      else {
        val partiallySorted = findMaximum(xs)
        iter(partiallySorted.tail, partiallySorted.head :: acc)
      }
    }

    iter(a, Nil)
  }
}