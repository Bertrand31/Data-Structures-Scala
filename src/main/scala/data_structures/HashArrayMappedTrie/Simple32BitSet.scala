package data_structures.hamt

import cats.implicits._

final case class Simple32BitSet(bitset: Int = 0) {
  def +(number: Int): Simple32BitSet =
    Simple32BitSet(bitset | (1 << number))

  def -(number: Int): Simple32BitSet =
    Simple32BitSet(bitset & (~1 << number))

  def contains(number: Int): Boolean =
    (bitset & (~(1 << number))) =!= bitset

  def getPosition(number: Int): (Int, Boolean) = {
    val binaryString = bitset.toBinaryString
    val rightmostBits = binaryString.drop(binaryString.size - number + 1)
    val isSet = rightmostBits.head === '1'
    val position = rightmostBits.tail.count(_ === '1')
    (position, isSet)
  }

  private val IntBits = Int.MaxValue.toBinaryString.size

  def toArray: IndexedSeq[Int] =
    (0 to IntBits).filter(bit => (bitset & ~(1 << bit)) =!= bitset)
}
