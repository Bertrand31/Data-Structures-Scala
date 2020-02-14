package data_structures

import cats.implicits._

case class BitSet(words: Array[Long]) {

  import BitSet.getWordIndex

  private def addToWord(nb: Long, wordIndex: Int): Array[Long] = {
    val updatedWord = this.words(wordIndex) | (1L << nb - (wordIndex.toLong << 6L))
    this.words.updated(wordIndex, updatedWord)
  }

  private def removeFromWord(nb: Long, wordIndex: Int): Array[Long] = {
    val updatedWord = this.words(wordIndex) & (~1L << nb)
    this.words.updated(wordIndex, updatedWord)
  }

  def +(number: Long): BitSet =
    BitSet(this.addToWord(number, getWordIndex(number)))

  def `++`: IterableOnce[Long] => BitSet =
    _.iterator.foldLeft(this)(_ + _)

  def -(number: Long): BitSet =
    BitSet(this.removeFromWord(number, getWordIndex(number)))

  def member(number: Long): Boolean = {
    val word = words(getWordIndex(number))
    (word & (~(1L << number))) =!= word
  }

  def toArray: Array[Long] =
    (this.words zip Iterator.from(0)).flatMap({
      case (word, wordIndex) =>
        word
          .toBinaryString
          .reverse
          .zip(Iterator from 0)
          .collect({ case ('1', index) => index + 0L.max(wordIndex.toLong << 6L) })
    })

  def cardinality: Int =
    this.words
      .map(_.toBinaryString.count(_ === '1'))
      .sum
}

object BitSet {

  def getWordIndex(nb: Long): Int = (nb >> 6L).toInt

  def apply(maxSize: Int): BitSet = new BitSet(new Array[Long](getWordIndex(maxSize) + 1))
}

object BitSetTests extends App {

  import scala.util.Random.between

  val bs = BitSet(2000)
  val sample = (0 until 1000).map(_ => between(0, 2000).toLong).distinct
  val bsWithData = bs ++ sample
  assert(bsWithData.toArray.toList.sorted == sample.sorted)
  assert(bsWithData.cardinality === sample.length)
}
