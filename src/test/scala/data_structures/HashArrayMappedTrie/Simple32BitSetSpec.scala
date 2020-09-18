import data_structures.hamt.Simple32BitSetContainer.Simple32BitSet

import org.scalatest.flatspec.AnyFlatSpec

class Simple32BitSetSpec extends AnyFlatSpec {

  behavior of "The Simple32BitSet implementation"

  behavior of "the apply method"

  val empty = Simple32BitSet()

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

  behavior of "the contains method"

  it should "return whether the bitset contains the given number" in {

    val withThree = empty add 3
    assert(withThree.contains(3))
    assert(!withThree.contains(4))
  }

  behavior of "the CountOnesUpTo method"

  it should "count the number of set bits up to a certain bit positoin" in {

    val bs = empty add 7 add 4 add 2
    assert(bs.countOnesUpTo(12) === 3)
    assert(bs.countOnesUpTo(6) === 2)
    assert(bs.countOnesUpTo(1) === 0)
  }

  behavior of "the getPosition method"

  it should "return how many bits are set before the one in question, and whether it is set" in {

    val populated = empty add 3 add 4 add 5 add 10
    assert(populated.getPosition(5) == ((2, true)))
    assert(populated.getPosition(10) == ((3, true)))
    assert(populated.getPosition(2) == ((0, false)))
    assert(populated.getPosition(6) == ((3, false)))
  }
}
