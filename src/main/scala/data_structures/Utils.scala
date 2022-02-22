package data_structures

import scala.reflect.ClassTag
import scala.collection.immutable.ArraySeq

object Utils {

  implicit class AugmentedArray[A: ClassTag](arr: Array[A]) {

    def updatedWith(index: Int, fn: A => A): Array[A] =
      arr.updated(index, fn(arr(index)))

    def insertAt(index: Int, elem: A): Array[A] = {
      val (front, back) = arr.splitAt(index)
      (front :+ elem) ++ back
    }
  }

  implicit class AugmentedArraySeq[A: ClassTag](arr: ArraySeq[A]) {

    def insertAt(index: Int, elem: A): ArraySeq[A] = {
      val newArr = new Array[A](arr.size + 1)
      var i = 0
      if (index >= arr.size) {
        while (i < arr.size) {
          newArr.update(i, arr(i))
          i += 1
        }
        newArr.update(arr.size, elem)
      } else {
        while (i < index) {
          newArr.update(i, arr(i))
          i += 1
        }
        newArr.update(index, elem)
        i = index + 1
        while (i < (newArr.size)) {
          newArr.update(i, arr(i - 1))
          i += 1
        }
      }
      ArraySeq.unsafeWrapArray(newArr)
    }

    def removeAt(index: Int): ArraySeq[A] =
      arr.take(index) ++ arr.drop(index + 1)
  }

  def log2: Double => Double = Math.log10(_) / Math.log10(2)
}
