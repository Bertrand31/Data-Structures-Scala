import data_structures.BloomFilter

import org.scalatest.flatspec.AnyFlatSpec

class BloomFilterSpec extends AnyFlatSpec {

  behavior of "The Bloom filter implementation"

  behavior of "the apply method"

  val empty = BloomFilter[Int](100000, 0.0001f)

  it should "create an empty bloom filter" in {

    assert(empty.isEmpty)
  }

  behavior of "the + (add) method"

  val withOne = empty + 4

  it should "add an element to the bloom filter" in {

    assert(!withOne.isEmpty)
  }

  behavior of "the mayContain method"

  val withThree = withOne ++ Vector(5, 2910)

  it should "return accurately whether the bloom filter contains en elements or not" in {
    assert(withThree mayContain 4)
    assert(withThree mayContain 5)
    assert(withThree mayContain 2910)
    assert(!withThree.mayContain(211))
  }

  behavior of "the approxNumberOfItems method"

  it should "return the correct number of elements" in {
    assert(withThree.approxNumberOfItems === 3)
  }
}
