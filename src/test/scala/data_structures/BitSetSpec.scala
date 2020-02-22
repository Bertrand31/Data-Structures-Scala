import data_structures.BitSet

import org.scalatest.flatspec.AnyFlatSpec

class BitSetSpec extends AnyFlatSpec {

  behavior of "The BitSet implementation"

  behavior of "the apply method"

  val empty = BitSet(1000)

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

  behavior of "the toArray method"

  it should "restitute the inserted numbers" in {

    val bs = BitSet(2000)
    val sample = (0 until 1000).map(_ => scala.util.Random.between(0, 2000).toLong).distinct
    val bsWithData = bs ++ sample
    assert(bsWithData.toArray.toList.sorted == sample.sorted)
    assert(bsWithData.cardinality === sample.length)
  }
}
