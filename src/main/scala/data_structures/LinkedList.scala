package data_structures

sealed trait LinkedList[+A] {

  final def apply: LinkedList[A] = Empty

  def +:[U >: A](elem: U): LinkedList[U] = Node(elem, this)

  def find[U >: A](predicate: U => Boolean): Option[U] =
    this match {
      case Empty => None
      case node: Node[A] =>
        if (predicate(node.value)) Some(node.value)
        else node.next.find(predicate)
    }

  // def ++:[U >: A](seq: TraversableOnce[U]): LinkedList[U] = seq.foldLeft(this)(_ +: _)
}

final case class Node[A](value: A, next: LinkedList[A]) extends LinkedList[A]
final case object Empty extends LinkedList[Nothing]

object LinkedListTest extends App {

  val list = Node("test", Node("foo", Node("bar", Empty)))
  assert(list.find(_ == "bar").isDefined)
  assert(list.find(_ == "test").isDefined)
  assert(list.find(_ == "baz").isEmpty)
}
