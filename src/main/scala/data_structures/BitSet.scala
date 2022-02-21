package data_structures

import scala.collection.immutable.ArraySeq
import cats.implicits._
import Utils.log2

object BitSetUtils {

    val LongBits = log2(Long.MaxValue).toInt

    def countOnes(number: Long): Int =
      (0 to LongBits).count(shift =>
        (number & ~(1L << shift)) =!= number
      )

    def getWordIndex(nb: Long): Int = (nb >> 6L).toInt
}


final case class BitSet(words: ArraySeq[Long]) {

  def add(number: Long): BitSet = {
    require(number >= 0, "Bitset element must be >= 0")
    val wordIndex = BitSetUtils.getWordIndex(number)
    val newWords = {
      val overflow = (wordIndex + 1) - words.size
      if (overflow > 0) words ++ new Array[Long](overflow)
      else words
    }
    val updatedWord = newWords(wordIndex) | (1L << number - (wordIndex.toLong << 6L))
    BitSet(newWords.updated(wordIndex, updatedWord))
  }

  def `++`: IterableOnce[Long] => BitSet =
    _.iterator.foldLeft(this)(_ add _)

  def remove(number: Long): BitSet = {
    require(number >= 0, "Bitset element must be >= 0")
    val wordIndex = BitSetUtils.getWordIndex(number)
    words.lift(wordIndex) match {
      case None => this
      case Some(word) =>
        val updatedWord = word & (~1L << number)
        val newWords = words.updated(wordIndex, updatedWord)
        BitSet(newWords.head +: newWords.tail.takeWhile(_ > 0))
    }
  }

  def contains(number: Long): Boolean =
    this.words
      .lift(BitSetUtils.getWordIndex(number))
      .exists(word => (word >>> number & 1) === 1)

  def iterator: Iterator[Int] =
    this.words.iterator.zipWithIndex flatMap {
      case (word, wordIndex) =>
        (0 to BitSetUtils.LongBits)
          .filter(bit => (word >>> bit & 1) === 1)
          .map(_ + (wordIndex << 6))
    }

  def toArray: Array[Int] = iterator.toArray

  def cardinality: Int =
    this.words.foldMap(BitSetUtils.countOnes)

  def isEmpty: Boolean =
    this.words.forall(_ === 0)
}

// A builder needs to be defined without using a companion object, because of newtype.
object BitSet {

  def apply(numbers: Long*): BitSet = new BitSet(ArraySeq(0)) ++ numbers
}
