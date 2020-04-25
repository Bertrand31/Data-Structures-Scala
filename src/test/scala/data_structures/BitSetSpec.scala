import data_structures.BitSetUtils
import data_structures.BitSetContainer._

import org.scalatest.flatspec.AnyFlatSpec

class BitSetSpec extends AnyFlatSpec {

  behavior of "The BitSet implementation"

  behavior of "the apply method"

  val empty = BitSetBuilder()

  it should "create an emty bitset" in {

    assert(empty.isEmpty)
  }

  behavior of "the insert (+) method"

  it should "insert the given number" in {

    val withThree = empty add 3
    assert(!withThree.isEmpty)
    assert(withThree contains 3)
  }

  behavior of "the remove (-) method"

  it should "remove a given number" in {

    val withThree = empty add 3
    val emptied = withThree remove 3
    assert(!emptied.contains(3))
    assert(emptied.isEmpty)
  }

  behavior of "the cardinality method"

  it should "return the correct number of inserted integers" in {

    val bs = BitSetBuilder() add 511
    assert(bs.cardinality === 1)
  }

  behavior of "the toArray method"

  it should "restitute the inserted numbers" in {

    val bs = BitSetBuilder()
    val sample = (0 until 1000).map(_ => scala.util.Random.between(0, 2000).toLong).distinct
    val bsWithData = bs ++ sample
    assert(bsWithData.toArray.toList.sorted == sample.sorted)
    assert(bsWithData.cardinality === sample.length)
  }

  behavior of "the countOnes method"

  it should "return the number of 1s in the binary representation of a number" in {

    (0 to 10000).foreach(nb =>
      assert(BitSetUtils.countOnes(nb) === nb.toBinaryString.count(_ === '1'))
    )
  }
}
