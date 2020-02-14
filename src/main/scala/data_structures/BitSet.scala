package data_structures

import cats.implicits._

case class BitSet(nb: Long = 0) {

  def +(number: Long): BitSet =
    BitSet(this.nb | (1L << number))

  def `++`: IterableOnce[Long] => BitSet =
    _.iterator.foldLeft(this)(_ + _)

  def -(number: Long): BitSet =
    BitSet(this.nb & (~(1L << number)))

  def member(number: Long): Boolean =
    (this.nb & (~(1L << number))) =!= this.nb

  def toIndexedSeq: IndexedSeq[Int] =
    this.nb
      .toBinaryString
      .reverse
      .zip(Iterator from 0)
      .collect({ case ('1', index) => index })

  def cardinality: Int =
    this.nb.toBinaryString.count(_ === '1')
}

object BitSetTests extends App {

  val bs = BitSet() + 62
  assert(bs.member(62))
  assert(!bs.member(64))
  assert(bs.cardinality === 1)
  assert(bs.toIndexedSeq == IndexedSeq(62))
}
