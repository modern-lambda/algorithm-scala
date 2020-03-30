package Search

object LinearSearch {
  def linearSearch(arr: List[Int], elem: Int): Int = {
    for (i <- arr.indices if (arr(i) == elem)) {
      return i
    }
    -1
  }
}
