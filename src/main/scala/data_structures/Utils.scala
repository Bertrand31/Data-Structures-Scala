package data_structures

import scala.reflect.ClassTag

object Utils {

  implicit class AugmentedArray[A: ClassTag](val arr: Array[A]) {

    def updatedWith(index: Int, fn: A => A): Array[A] =
      arr.updated(index, fn(arr(index)))

    def insertAt(index: Int, elem: A): Array[A] = {
      val (front, back) = arr.splitAt(index)
      (front :+ elem) ++ back
    }

    def removeAt(index: Int): Array[A] =
      arr.take(index) ++ arr.drop(index + 1)
  }

  def log2: Long => Double = Math.log10(_) / Math.log10(2)
}
