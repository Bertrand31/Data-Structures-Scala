package data_structures.trie

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

    def getPosition(item: Int): (Int, Boolean) = {
      def isSet = bitmap contains item
      val onesUpToItem = bitmap.iterator.asScala.takeWhile(_ < item).size
      (onesUpToItem, isSet)
    }

    def toList: List[Int] = bitmap.iterator.asScala.map(_.toInt).toList
  }
}
