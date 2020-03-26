package Sorts

object BubbleSort {
  def bubbleSort(array: Array[Int]): Array[Int] = {
    for (i <- 0 until array.length - 1) {
      for (j <- 0 until array.length - 1 - i) {
        if (array(j) > array(j + 1)) {
          val tmp = array(j)
          array(j) = array(j + 1)
          array(j + 1) = tmp
        }
      }
    }
    array
  }
}
