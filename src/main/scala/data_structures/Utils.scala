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

  implicit class AugmentedArraySeq[A](arr: ArraySeq[A]) {

    def updatedWith(index: Int, fn: A => A): ArraySeq[A] =
      arr.updated(index, fn(arr(index)))

    def insertAt(index: Int, elem: A): ArraySeq[A] = {
      val (front, back) = arr.splitAt(index)
      (front :+ elem) ++ back
    }

    def removeAt(index: Int): ArraySeq[A] =
      arr.take(index) ++ arr.drop(index + 1)
  }

  def log2: Double => Double = Math.log10(_) / Math.log10(2)
}
