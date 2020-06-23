import scala.annotation.tailrec

object Sorting {

  def insertionSort(arr: Array[Int]): Array[Int] = {
    val a = arr.clone
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

  def selectionSort(arr: Array[Int]): Array[Int] = {
    val a = arr.clone
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
        if (elem > acc.head)  elem :: acc
        else                  acc.head :: elem :: acc.tail
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

  def mergeSort(arr: Array[Int]): Array[Int] = ???

  def mergeSortFunctional(a: List[Int]): List[Int] = {
    @tailrec
    def merge(left: List[Int], right: List[Int], acc: List[Int]): List[Int] = (left, right) match {
      case (Nil, _) => acc ++ right
      case (_, Nil) => acc ++ left
      case (x :: xs, y :: ys) =>
        if (x < y)  merge(xs, right, acc :+ x)
        else        merge(left, ys, acc :+ y)
    }

    a match {
      case Nil => Nil
      case xs :: Nil => List(xs)
      case _ =>
        val (left, right) = a splitAt a.length/2
        merge(mergeSortFunctional(left), mergeSortFunctional(right), List.empty[Int])
    }
  }
}