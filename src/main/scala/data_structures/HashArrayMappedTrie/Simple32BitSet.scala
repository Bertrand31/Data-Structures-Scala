package data_structures.hamt

import scala.language.implicitConversions
import cats.implicits._
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops.toCoercibleIdOps

package object Simple32BitSetContainer {

  @newtype final case class Simple32BitSet(val word: Int = 0) {

    def add(number: Int): Simple32BitSet =
      (this.word | (1 << number)).coerce

    def remove(number: Int): Simple32BitSet =
      (this.word & (~1 << number)).coerce

    def contains(number: Int): Boolean =
      ((this.word >>> number) & 1) === 1

    private def countOnesUpTo(maxBinaryDigit: Int): Int = {
      var i = 0
      var ones = 0
      var number = this.word
      while (i < maxBinaryDigit) {
        ones += number & 1
        number = number >>> 1
        i += 1
      }
      ones
    }

    def getPosition(number: Int): (Int, Boolean) =
      (countOnesUpTo(number), contains(number))

    def isEmpty: Boolean = this.word === 0
  }

  object Simple32BitSet {

    def apply(): Simple32BitSet = 0.coerce

    def empty: Simple32BitSet = this.apply()
  }
}
