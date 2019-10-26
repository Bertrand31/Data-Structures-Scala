package data_structures

sealed trait LinkedList[+A] {

  final def apply: LinkedList[A] = Empty

  def +:[U >: A](elem: U): LinkedList[U] = Node(elem, this)

  // def ++:[U >: A](seq: TraversableOnce[U]): LinkedList[U] = seq.foldLeft(this)(_ +: _)
}

final case class Node[A](value: A, next: LinkedList[A]) extends LinkedList[A]
final case object Empty extends LinkedList[Nothing]
