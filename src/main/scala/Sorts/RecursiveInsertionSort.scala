package Sorts

object RecursiveInsertionSort {
  def recursiveInsertionSort(array: List[Int]): List[Int] = {

    def insertion(x: List[Int]): List[Int] = {
      x match {
        case List() => List()
        case x :: xs => ins(x, insertion(xs))
      }
    }

    def ins(x: Int, xs: List[Int]): List[Int] = {
      xs match {
        case List() => List(x)
        case x2 :: xs2 => if (x <= x2) x :: xs else x2 :: ins(x, xs2)
      }
    }

    insertion(array)
  }
}
