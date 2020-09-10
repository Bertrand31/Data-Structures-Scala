package data_structures

import scala.jdk.CollectionConverters._
import scala.util.chaining.scalaUtilChainingOps
import org.roaringbitmap.RoaringBitmap

object RoaringBitmapUtils {

  implicit class AugmentedBitmap(val bitmap: RoaringBitmap) {

    def +(item: Int): RoaringBitmap = bitmap.tap(_ add item)

    def -(item: Int): RoaringBitmap = bitmap.tap(_ remove item)

    private lazy val iterator: Iterator[Integer] = bitmap.iterator.asScala

    def getPosition(item: Int): (Int, Boolean) = {
      def isSet = bitmap contains item
      val onesUpToItem = iterator.takeWhile(_ < item).size
      (onesUpToItem, isSet)
    }

    def toIterator: Iterator[Int] = iterator.map(_.toInt)
  }
}
