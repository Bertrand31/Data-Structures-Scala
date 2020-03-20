package data_structures.yab

import cats.implicits._
import data_structures.Utils.AugmentedArray

final case class YetAnotherBitset(
  private val bitset: Simple64BitSet = Simple64BitSet(),
  private val words: Array[Long] = Array.empty,
) {

  import YetAnotherBitset._

  def +(number: Long): YetAnotherBitset = {
    require(number >= 0, "Bitset element must be >= 0")
    val wordIndex = getWordIndex(number)
    val (position, isSet) = this.bitset.getPosition(wordIndex)
    val oldWord = if (isSet) this.words(position) else 0
    val updatedWord = oldWord | (1L << number - (wordIndex.toLong << 6L))
    val newWords = {
      if (isSet) this.words.updated(position, updatedWord)
      else this.words.insertAt(position,updatedWord)
    }
    val newBitset = if (isSet) this.bitset else this.bitset + wordIndex
    YetAnotherBitset(bitset=newBitset, words=newWords)
  }

  def `++`: IterableOnce[Long] => YetAnotherBitset =
    _.iterator.foldLeft(this)(_ + _)

  def -(number: Long): YetAnotherBitset = {
    require(number >= 0, "Bitset element must be >= 0")
    val wordIndex = getWordIndex(number)
    val (position, isSet) = this.bitset.getPosition(wordIndex)
    if (!isSet) this
    else {
      val newWords = this.words.removeAt(position)
      val newBitset = this.bitset - wordIndex
      YetAnotherBitset(bitset=newBitset, words=newWords)
    }
  }

  def contains(number: Long): Boolean = {
    require(number >= 0, "Bitset element must be >= 0")
    val wordIndex = getWordIndex(number)
    val (position, isSet) = this.bitset.getPosition(wordIndex)
    if (!isSet) false
    else {
      val word = this.words(position)
      (word & (~(1L << number))) =!= word
    }
  }

  def toIndexedSeq: IndexedSeq[Long] =
    this.bitset.toIndices
      .zip(this.words)
      .flatMap {
        case (wordIndex, word) =>
          (0 to LongBits)
            .filter(bit => (word & ~(1L << bit)) =!= word)
            .map(_ + (wordIndex.toLong << 6L))
      }

  def cardinality: Int =
    this.words.foldLeft(0)(_ + countOnes(_))

  def isEmpty: Boolean =
    this.bitset.isEmpty
}

object YetAnotherBitset {

  private val LongBits = Long.MaxValue.toBinaryString.length

  def countOnes(number: Long): Int =
    (0 to LongBits).count(shift =>
      (number & ~(1L << shift)) =!= number
    )

  def getWordIndex(nb: Long): Int = (nb >> 6L).toInt

  def apply(initialItems: IterableOnce[Long]): YetAnotherBitset =
    new YetAnotherBitset ++ initialItems

  def apply(numbers: Long*): YetAnotherBitset = this.apply(numbers)
}
