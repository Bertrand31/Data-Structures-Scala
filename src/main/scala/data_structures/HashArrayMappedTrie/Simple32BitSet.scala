package data_structures.hamt

import cats.implicits._

final case class Simple32BitSet(private val word: Int = 0) {

  def +(number: Int): Simple32BitSet =
    Simple32BitSet(this.word | (1 << number))

  def -(number: Int): Simple32BitSet =
    Simple32BitSet(this.word & (~1 << number))

  def contains(number: Int): Boolean =
    (this.word & (~(1 << number))) =!= this.word

  private def countOnesUpTo(maxBinaryDigit: Int): Int =
    (0 until maxBinaryDigit).count(shift =>
      (this.word & ~(1L << shift)) =!= this.word
    )

  def getPosition(number: Int): (Int, Boolean) =
    (countOnesUpTo(number), contains(number))

  def isEmpty: Boolean = this.word === 0
}
