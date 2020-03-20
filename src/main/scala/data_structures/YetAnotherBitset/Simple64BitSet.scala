package data_structures.yab

import cats.implicits._

final case class Simple64BitSet(private val word: Long = 0) {

  def +(number: Int): Simple64BitSet =
    Simple64BitSet(this.word | (1 << number))

  def -(number: Int): Simple64BitSet =
    Simple64BitSet(this.word & (~1 << number))

  def contains(number: Int): Boolean =
    (this.word & (~(1 << number))) =!= this.word

  private def countOnesUpTo(maxBinaryDigit: Int): Int =
    (0 until maxBinaryDigit).count(shift =>
      (this.word & ~(1L << shift)) =!= this.word
    )

  def getPosition(number: Int): (Int, Boolean) =
    (countOnesUpTo(number), contains(number))

  def isEmpty: Boolean = this.word === 0

  private val LongBits = Long.MaxValue.toBinaryString.length

  def toIndices: IndexedSeq[Int] =
    (0 to LongBits).collect {
      case bit if (this.word & ~(1L << bit)) =!= this.word => bit
    }
}
