import scala.annotation.tailrec

object Searching {

  def linearSearch(arr: Array[Int], v: Int): Option[Int] = {
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

  def linearSearchFunctional(a: List[Int], v: Int): Option[Int] = {
    @tailrec
    def iter(xs: List[Int], index: Int = 0): Option[Int] = xs match {
      case Nil => None
      case h :: t =>
        if (h == v) Some(index)
        else        iter(t, index + 1)
    }

    iter(a)
  }
}
