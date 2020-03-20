import data_structures.yab.YetAnotherBitset

import org.scalatest.flatspec.AnyFlatSpec

class YetAnotherBitSetSpec extends AnyFlatSpec {

  behavior of "The BitSet implementation"

  behavior of "the apply method"

  val empty = YetAnotherBitset()

  it should "create an emty bitset" in {

    assert(empty.isEmpty)
  }

  behavior of "the insert (+) method"

  it should "insert the given number" in {

    val withThree = empty + 3
    assert(!withThree.isEmpty)
    assert(withThree contains 3)
  }

  behavior of "the remove (-) method"

  it should "remove a given number" in {

    val withThree = empty + 3
    val emptied = withThree - 3
    assert(!emptied.contains(3))
    assert(emptied.isEmpty)
  }

  behavior of "the cardinality method"

  it should "return the correct number of inserted integers" in {

    val bs = YetAnotherBitset() + 511
    assert(bs.cardinality === 1)
  }

  behavior of "the toIndexedSeq method"

  it should "restitute the inserted numbers" in {

    val bs = YetAnotherBitset()
    val sample = (0 until 1000).map(_ => scala.util.Random.between(0, 2000).toLong).distinct
    val bsWithData = bs ++ sample
    assert(bsWithData.toIndexedSeq.sorted == sample.sorted)
    assert(bsWithData.cardinality === sample.length)
  }

  behavior of "the countOnes method"

  it should "return the number of 1s in the binary representation of a number" in {

    (0 to 10000).foreach(nb =>
      assert(YetAnotherBitset.countOnes(nb) === nb.toBinaryString.count(_ === '1'))
    )
  }
}
