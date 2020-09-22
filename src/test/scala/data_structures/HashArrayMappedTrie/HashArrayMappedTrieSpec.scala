package data_structures.hamt

import data_structures.hamt.HashArrayMappedTrie

import org.scalatest.flatspec.AnyFlatSpec

class HashArrayMappedTrieSpec extends AnyFlatSpec {

  behavior of "The Hash Array Mapped Trie (HAMT) implementation"

  behavior of "the empty method"

  it should "create an emty HAMT" in {

    val empty = HashArrayMappedTrie.empty[Int, String]
    assert(empty.isEmpty)
  }

  behavior of "the insert (+) method"

  it should "insert the given pair" in {

    val withThree = HashArrayMappedTrie[Int, String]() + (3 -> "foo")
    assert(!withThree.isEmpty)
    assert(withThree has 3)
  }

  behavior of "the insert multiple (++) method"

  it should "insert the given pairs" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    assert(!populated.isEmpty)
    assert(populated has 3)
    assert(populated has 4)
    assert(populated has 5)
  }

  behavior of "the remove (-) method"

  it should "remove the pair corresponding to the given key" in {

    val withThree = HashArrayMappedTrie[Int, String]() + (3 -> "foo")
    val empty = withThree - 3
    assert(empty.isEmpty)
  }

  behavior of "the remove multiple (--) method"

  it should "remove the pairs corresponding to the given keys" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    val empty = populated -- Seq(3, 4, 5)
    assert(empty.isEmpty)
  }

  behavior of "the get method"

  it should "return an Option containg the value of the given index" in {

    val populated = HashArrayMappedTrie[Int, String]() + (3 -> "foo")
    assert(populated.get(3) == Some("foo"))
    assert(populated.get(4) == None)
  }

  behavior of "the size method"

  it should "return the number of inserted tuples" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    assert(populated.size === 3)
  }

  behavior of "the toArray method"

  it should "return the inserted elements" in {

    val values = Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    val populated = HashArrayMappedTrie[Int, String]() ++ values
    assert(populated.toArray.toSeq.sorted === values.sorted)
  }

  behavior of "the keys method"

  it should "return a view of the keys of the inserted tuples" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    assert(populated.keys.toList.sorted === List(3, 4, 5))
  }

  behavior of "the values method"

  it should "return a view of the values of the inserted tuples" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    assert(populated.values.toList.sorted === List("bar", "baz", "foo"))
  }

  behavior of "the count method"

  it should "return the number of tuples satisfying a predicate" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    assert(populated.count(_._2.startsWith("ba")) === 2)
  }

  behavior of "the countValues method"

  it should "return the number of values satisfying a predicate" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    assert(populated.countValues(_.startsWith("ba")) === 2)
  }

  behavior of "the find method"

  it should "return the first tuple satisfying a predicate" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    assert(populated.find(_._2.startsWith("ba")) === Some((4 -> "bar")))
  }

  behavior of "the findValue method"

  it should "return the first value satisfying a predicate" in {

    val populated = HashArrayMappedTrie[Int, String]() ++ Seq((3 -> "foo"), (4 -> "bar"), (5 -> "baz"))
    assert(populated.findValue(_.startsWith("ba")) === Some("bar"))
  }
}
