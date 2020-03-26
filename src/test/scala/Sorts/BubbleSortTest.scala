package Sorts

import org.scalatest._

class BubbleSortTest extends FlatSpec {
  val arr = Array(3,2,7,1,9,0)
  assert(BubbleSort.bubbleSort(arr) === Array(0,1,2,3,7,9))
}
