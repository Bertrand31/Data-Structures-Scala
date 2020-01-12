import data_structures.Empty

import org.scalatest.flatspec.AnyFlatSpec

class LinkedListSpec extends AnyFlatSpec {

  behavior of "The LinkedList implementation"

  behavior of "the +: (prepend) method"

  val list = "test" +: "foo" +: "bar" +: Empty

  it should "prepend elements to the head of the list" in {
    assert(list.length == 3)
  }

  behavior of "the map method"

  it should "apply the given unary function to all values held by the list" in {
    assert(list.map(_.length).toString == "List(4, 3, 3)")
    assert(list.map(_ == "test").toString == "List(true, false, false)")
  }

  behavior of "the headOption method"

  it should "return a Some with the head value when the list is not empty" in {
    assert(list.headOption == Some("test"))
  }

  it should "return a None when the list is empty" in {
    assert(Empty.headOption == None)
  }

  behavior of "the lastOption method"

  it should "return a Some with the last value when the list is not empty" in {
    assert(list.lastOption == Some("bar"))
  }

  it should "return a None when the list is empty" in {
    assert(Empty.lastOption == None)
  }

  behavior of "the find method"

  it should "return a Some with the first value satisfying the predicate" in {
    assert(list.find(_ == "bar").get == "bar")
    assert(list.find(_.length == 4).get == "test")
  }

  it should "return a None when no value satisfies the predicate" in {
    assert(list.find(_ == "baz").isEmpty)
  }

  behavior of "the count method"

  it should "return the number of times the predicate returned true over the list" in {
    assert(list.count(_.length == 3) == 2)
    assert(list.count(_ == "baz") == 0)
  }

  behavior of "the exists method"

  it should "return whether at least one element of the list satisfies the predicate" in {
    assert(list.exists(_.length == 4))
    assert(!list.exists(_.length == 5))
  }

  behavior of "the forall method"

  it should "return whether all elements of the list satisfy the predicate" in {
    assert(list.forall(_.length < 5))
  }

  behavior of "the reverse method"

  it should "reverse the order of a list" in {
    assert(list.reverse.toString == "List(bar, foo, test)")
    assert(Empty.reverse.toString == "List()")
  }
}
