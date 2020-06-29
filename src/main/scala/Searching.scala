import scala.annotation.tailrec
import scala.util.control.Breaks._

object Searching {

  type Pos = Int

  def linearSearch(arr: Array[Int], v: Int): Option[Pos] = {
    // apply sentinel pattern to reduce comparisons
    val sentinel = v
    val a = arr.clone :+ sentinel

    var pos = 0
    while (a(pos) != sentinel) {
      pos += 1
    }

    if (pos < arr.length) Some(pos)
    else                  None
  }

  def linearSearchFunctional(a: List[Int], v: Int): Option[Pos] = {
    @tailrec
    def iter(xs: List[Int], index: Int = 0): Option[Int] = xs match {
      case Nil => None
      case h :: t =>
        if (h == v) Some(index)
        else        iter(t, index + 1)
    }

    iter(a)
  }

  def binarySearchIter(arr: Array[Int], v: Int): Option[Pos] = {
    // assumes input collection is sorted ascending

    var (low, mid, high) = (0, 0, arr.length-1)
    var result = -1
    breakable {
      while (low <= high) {
        mid = low + (high - low) / 2
        if (v == arr(mid))      { result = mid; break }
        else if (v > arr(mid))  low = mid + 1
        else                    high = mid - 1
      }
    }

    result match {
      case -1 => None
      case i => Some(i)
    }
  }

  def binarySearchRec(arr: Array[Int], v: Int): Option[Pos] = {
    // assumes input collection is sorted ascending

    // doing a specifically functional version does not make sense
    // just show recursive version since it's the same

    @tailrec
    def iter(low: Int, high: Int): Option[Int] = {
      if (low > high) None
      else {
        val mid = low + (high - low) / 2
        if (v == arr(mid))      Some(mid)
        else if (v > arr(mid))  iter(mid + 1, high)
        else                    iter(low, mid - 1)
      }
    }

    iter(0, arr.length-1)
  }

  // nevermind, I'm doing it!!
  @tailrec
  def binarySearchFunctional(a: List[Int], v: Int): Option[Pos] = a match {
    case Nil => None
    case _ =>
      val mid = a.length / 2
      if (v == a(mid))      Some(mid)
      else if (v > a(mid))  binarySearchFunctional(a drop mid, v)
      else                  binarySearchFunctional(a take mid, v)
  }
}
