package data_structures

import cats.implicits._

case class BitSet(nb: Long = 0) {

  def +(number: Int): BitSet =
    BitSet(this.nb | (1 << number))

  def -(number: Int): BitSet =
    BitSet(this.nb & (~(1 << number)))

  def member(number: Int): Boolean =
    (this.nb & (~(1 << number))) =!= this.nb

  def toIndexedSeq: IndexedSeq[Int] =
    this.nb
      .toBinaryString
      .reverse
      .zip(Iterator from 0)
      .collect({ case ('1', index) => index })
}

object BitSetTests extends App {

  val bs = BitSet()
  println(bs.nb)
  val bsWithThree = bs + 3
  println(bsWithThree.nb)
  println(bsWithThree member 4)
  println(bsWithThree member 3)
  println(bsWithThree.toIndexedSeq)
}
