package data_structures

import scala.annotation.tailrec

sealed trait LinkedList[+A] {

  final def apply: LinkedList[A] = Empty

  def +:[U >: A](elem: U): LinkedList[U] = Node(elem, this)

  def map[B](fn: A => B): LinkedList[B] =
    this match {
      case Empty => Empty
      case Node(value, next) => Node(fn(value), next.map(fn))
    }

  @tailrec
  def find[U >: A](predicate: U => Boolean): Option[U] =
    this match {
      case Empty => None
      case Node(value, next) =>
        if (predicate(value)) Some(value)
        else next.find(predicate)
    }

  @tailrec
  def tap[U >: A](fn: U => Unit): LinkedList[U] =
    this match {
      case Empty => Empty
      case Node(value, next) => {
        fn(value)
        next.tap(fn)
      }
    }

  @tailrec
  def exists[U >: A](predicate: U => Boolean): Boolean =
    this match {
      case Empty => false
      case Node(value, next) =>
        if (predicate(value)) true
        else next.exists(predicate)
    }

  @tailrec
  def forall[U >: A](predicate: U => Boolean, soFar: Boolean = true): Boolean =
    this match {
      case Empty => soFar
      case Node(value, next) =>
        if (predicate(value)) next.forall(predicate, soFar)
        else false
    }

  @tailrec
  def count[U >: A](predicate: U => Boolean, soFar: Int = 0): Int =
    this match {
      case Empty => soFar
      case Node(value, next) =>
        next.count(predicate, if (predicate(value)) soFar + 1 else soFar)
    }

  @tailrec
  private def reverseInternal[U >: A](soFar: LinkedList[U] = Empty): LinkedList[U] =
    this match {
      case Empty => soFar
      case Node(value, next) => next.reverseInternal(Node(value, soFar))
    }

  def reverse[U >: A]: LinkedList[U] = this.reverseInternal()

  @tailrec
  private def lengthInternal(soFar: Int = 0): Int =
    this match {
      case Empty => soFar
      case Node(_, next) => next.lengthInternal(soFar + 1)
    }

  lazy val length = this.lengthInternal()

  @tailrec
  private def toStringInternal(soFar: String = ""): String =
    this match {
      case Empty => soFar
      case Node(value, next) => {
        val newString = if (soFar.isEmpty) value.toString else s"$soFar, $value"
        next.toStringInternal(newString)
      }
    }

  override def toString: String = "List(" + this.toStringInternal() + ")"
}

final case class Node[A](value: A, next: LinkedList[A]) extends LinkedList[A]
final case object Empty extends LinkedList[Nothing]

object LinkedListTest extends App {

  val list = Node("test", Node("foo", Node("bar", Empty)))
  assert(list.find(_ == "bar").isDefined)
  assert(list.find(_ == "test").isDefined)
  assert(list.find(_ == "baz").isEmpty)
  assert(list.length == 3)
  assert(list.count(_.length == 3) == 2)
  assert(list.exists(_.length == 4))
  assert(!list.exists(_.length == 5))
  assert(list.forall(_.length < 5))
  assert(list.toString == "List(test, foo, bar)")
  assert(list.reverse.toString == "List(bar, foo, test)")
  assert(list.map(_.length).toString == "List(4, 3, 3)")
}
