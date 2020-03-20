import data_structures.Utils.AugmentedArray

import org.scalatest.flatspec.AnyFlatSpec

class ArrayUtilsSpec extends AnyFlatSpec {

  behavior of "The ArrayUtils array methods"

  behavior of "the insertAt method"

  it should "insert an element at the given index" in {

    val arr = Array(1, 2, 4, 5)
    assert(arr.insertAt(2, 3).toList == Array(1, 2, 3, 4, 5).toList)
  }

  it should "insert an element at the end of the array if the index is >= to the length" in {

    val arr = Array(1, 2, 3, 4)
    assert(arr.insertAt(4, 10).toList == Array(1, 2, 3, 4, 10).toList)
    assert(arr.insertAt(5, 10).toList == Array(1, 2, 3, 4, 10).toList)
  }

  it should "insert an element at the beginning of the array if the index is 0" in {

    val arr = Array(1, 2, 3, 4)
    assert(arr.insertAt(0, 0).toList == Array(0, 1, 2, 3, 4).toList)
  }

  behavior of "the removeAt method"

  it should "remove the element at the given index, and return a shorter array" in {

    val arr = Array(1, 2, 3, 4)
    assert(arr.removeAt(2).toList == List(1, 2, 4))
  }

  it should "return the array as is if the index is >= to the length" in {

    val arr = Array(1, 2, 3, 4)
    assert(arr.removeAt(5).toList == List(1, 2, 3, 4))
  }

  it should "remove the head of the array if the index is 0" in {

    val arr = Array(1, 2, 3, 4)
    assert(arr.removeAt(0).toList == List(2, 3, 4))
  }
}
