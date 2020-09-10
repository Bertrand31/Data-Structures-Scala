package data_structures.hamt

import scala.language.implicitConversions
import cats.implicits._
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops.toCoercibleIdOps

package object Simple32BitSetContainer {

  @newtype final case class Simple32BitSet(val word: Int = 0) {

    def add(number: Int): Simple32BitSet =
      Simple32BitSet(this.word | (1 << number))

    def remove(number: Int): Simple32BitSet =
      Simple32BitSet(this.word & (~1 << number))

    def contains(number: Int): Boolean =
      (this.word & (~(1 << number))) =!= this.word

    private def countOnesUpTo(maxBinaryDigit: Int): Int = {
      var i = 0
      var ones = 0
      while (i < maxBinaryDigit) {
        if ((this.word & ~(1L << i)) =!= this.word) {
          ones += 1
        }
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
  }
}
