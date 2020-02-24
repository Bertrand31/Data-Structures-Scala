package data_structures.hamt

import cats.implicits._

final case class Simple32BitSet(bitset: Int = 0) {

  def +(number: Int): Simple32BitSet =
    Simple32BitSet(bitset | (1 << number))

  def -(number: Int): Simple32BitSet =
    Simple32BitSet(bitset & (~1 << number))

  def contains(number: Int): Boolean =
    (bitset & (~(1 << number))) =!= bitset

  private def countOnesUpTo(maxBinaryDigit: Int): Int =
    (0 until maxBinaryDigit).count(shift =>
      (bitset & ~(1L << shift)) =!= bitset
    )

  def getPosition(number: Int): (Int, Boolean) =
    (countOnesUpTo(number), contains(number))

  def isEmpty: Boolean = this.bitset === 0
}
