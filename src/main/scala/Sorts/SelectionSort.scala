package Sorts

object SelectionSort {
  def selectionSort(array: Array[Int]): Array[Int] = {
    for (i <- 0 to array.length - 1) {
      var min: Int = i
      var minVar = array(i)

      for (j <- i + 1 to array.length - 1) {
        if (array(j) < minVar) {
          min = j
          minVar = array(j)
        }
      }
      val temp: Int = array(i)
      array(i) = array(min)
      array(min) = temp
    }
    array
  }
}
