package data_structures

import scala.language.implicitConversions
import cats.implicits._
import io.estatico.newtype.macros.newtype

object BitSetUtils {

    def LongBits = Long.MaxValue.toBinaryString.length

    def countOnes(number: Long): Int =
      (0 to LongBits).count(shift =>
        (number & ~(1L << shift)) =!= number
      )

    def getWordIndex(nb: Long): Int = (nb >> 6L).toInt
}

package object BitSetContainer {

  import BitSetUtils._

  @newtype case class BitSet(words: Array[Long]) {

    def +(number: Long): BitSet = {
      require(number >= 0, "Bitset element must be >= 0")
      val wordIndex = getWordIndex(number)
      val newWords = {
        val overflow = (wordIndex + 1) - words.size
        if (overflow > 0) words ++ new Array[Long](overflow)
        else words
      }
      val updatedWord = newWords(wordIndex) | (1L << number - (wordIndex.toLong << 6L))
      BitSet(newWords.updated(wordIndex, updatedWord))
    }

    def `++`: IterableOnce[Long] => BitSet =
      _.iterator.foldLeft(this)(_ + _)

    def -(number: Long): BitSet = {
      require(number >= 0, "Bitset element must be >= 0")
      val wordIndex = getWordIndex(number)
      words.lift(wordIndex) match {
        case None => this
        case Some(word) =>
          val updatedWord = word & (~1L << number)
          val newWords = words.updated(wordIndex, updatedWord)
          BitSet(newWords.head +: newWords.tail.takeWhile(_ > 0))
      }
    }

    def contains(number: Long): Boolean =
      this.words.lift(getWordIndex(number))
        .fold(false)(word => (word & (~(1L << number))) =!= word)

    def toArray: Array[Long] =
      this.words.zipWithIndex flatMap {
        case (word, wordIndex) =>
          (0 to LongBits)
            .filter(bit => (word & ~(1L << bit)) =!= word)
            .map(_ + (wordIndex.toLong << 6L))
      }

    def cardinality: Int =
      this.words.foldLeft(0)(_ + countOnes(_))

    def isEmpty: Boolean =
      this.words.forall(_ === 0)
  }

  // A builder needs to be defined without using a companion object, because of newtype.
  object BitSetBuilder {

    def apply(numbers: Long*): BitSet = BitSet(Array(0)) ++ numbers
  }
}
