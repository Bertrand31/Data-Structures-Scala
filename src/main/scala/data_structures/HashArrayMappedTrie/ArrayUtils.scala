package data_structures.hamt

import scala.reflect.ClassTag

object ArrayUtils {

  implicit class AugmentedArray[A: ClassTag](val arr: Array[A]) {

    def insertAt(index: Int, elem: A): Array[A] = {
      val (front, back) = arr.splitAt(index)
      (front :+ elem) ++ back
    }
  }
}
