package data_structures

import cats.implicits._

case class BitSet(words: Array[Long] = Array(0)) {

  import BitSet._

  type Words = Array[Long]

  private def addToWord(nb: Long, wordIndex: Int, words: Words): Words = {
    val newWords = {
      val overflow = (wordIndex + 1) - words.size
      if (overflow > 0) words ++ new Array[Long](overflow)
      else words
    }
    val updatedWord = newWords(wordIndex) | (1L << nb - (wordIndex.toLong << 6L))
    newWords.updated(wordIndex, updatedWord)
  }

  private def removeFromWord(nb: Long, wordIndex: Int, words: Words): Words = {
    val updatedWord = words(wordIndex) & (~1L << nb)
    val newWords = words.updated(wordIndex, updatedWord)
    newWords.head +: newWords.tail.takeWhile(_ =!= 0)
  }

  def +(number: Long): BitSet =
    BitSet(this.addToWord(number, getWordIndex(number), words))

  def `++`: IterableOnce[Long] => BitSet =
    _.iterator.foldLeft(this)(_ + _)

  def -(number: Long): BitSet =
    BitSet(this.removeFromWord(number, getWordIndex(number), words))

  def contains(number: Long): Boolean = {
    val word = this.words(getWordIndex(number))
    (word & (~(1L << number))) =!= word
  }

  def toArray: Array[Long] =
    this.words.zip(Iterator from 0) flatMap {
      case (word, wordIndex) =>
        (0 to LongBits)
          .filter(bit => (word & ~(1L << bit)) =!= word)
          .map(_ + (wordIndex.toLong << 6L))
    }

  def cardinality: Int =
    this.words.map(countOnes).sum

  def isEmpty: Boolean =
    this.words.forall(_ === 0)
}

object BitSet {

  private val LongBits = Long.MaxValue.toBinaryString.length

  def countOnes(number: Long): Int =
    (0 to LongBits).count(shift =>
      (number & ~(1L << shift)) =!= number
    )

  def getWordIndex(nb: Long): Int = (nb >> 6L).toInt
}
