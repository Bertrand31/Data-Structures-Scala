package data_structures

import scala.jdk.CollectionConverters._
import org.roaringbitmap.RoaringBitmap

object RoaringBitmapUtils {

  implicit class AugmentedBitmap(val bitmap: RoaringBitmap) {

    def +(item: Int): RoaringBitmap = {
      bitmap add item
      bitmap
    }

    def -(item: Int): RoaringBitmap = {
      bitmap remove item
      bitmap
    }

    private def iterator: Iterator[Integer] = bitmap.iterator.asScala

    def getPosition(item: Int): (Int, Boolean) = {
      def isSet = bitmap contains item
      val onesUpToItem = iterator.takeWhile(_ < item).size
      (onesUpToItem, isSet)
    }

    def toIterator: Iterator[Int] = iterator.map(_.toInt)
  }
}
