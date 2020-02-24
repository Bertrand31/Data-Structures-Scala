package data_structures.hamt

import scala.util.hashing.MurmurHash3.stringHash

sealed trait HashArrayMappedTrie[+A, +B]

final case class Leaf[A, B](values: Array[(A, B)]) extends HashArrayMappedTrie[A, B]

final case class Node[A, B](
  bitset: Simple32BitSet = Simple32BitSet(),
  children: Array[HashArrayMappedTrie[A, B]] = new Array[HashArrayMappedTrie[A, B]](0),
) extends HashArrayMappedTrie[A, B] {

  private val StepBits = 5

  private def getPath: String => List[Int] =
    stringHash(_)
      .toBinaryString
      .reverse
      .toSeq
      .sliding(StepBits, StepBits)
      .map(_.unwrap)
      .map(Integer.parseInt(_, 2))
      .toList

  private def descendAndAdd(item: (A, B), steps: List[Int], current: Node[A, B]): Node[A, B] =
    steps match {
      case head +: Nil => {
        val (position, isSet) = current.bitset.getPosition(head)
        if (isSet) {
          val newChildren = current.children(position) match {
            case Leaf(values) => current.children.updated(position, Leaf(values :+ item))
            case _ => current.children.updated(position, Leaf(Array(item))) // Cannot happen
          }
          current.copy(children=newChildren)
        } else {
          val (front, back) = current.children.splitAt(position)
          val newChildren = (front :+ Leaf(Array(item))) ++ back
          val newBitSet = current.bitset + head
          current.copy(children=newChildren, bitset=newBitSet)
        }
      }
      case head +: tail => {
        val (position, isSet) = current.bitset.getPosition(head)
        if (isSet) {
          current.children(position) match {
            case node: Node[A, B] => {
              val newChild = descendAndAdd(item, tail, node)
              val newChildren = current.children.updated(position, newChild)
              current.copy(children=newChildren)
            }
            case _ => current // Cannot happen
          }
        } else {
          val newChild = descendAndAdd(item, tail, Node())
          val (front, back) = current.children.splitAt(position)
          val newChildren = (front :+ newChild) ++ back
          val newBitSet = current.bitset + head
          current.copy(children=newChildren, bitset=newBitSet)
        }
      }
    }

  def +(item: (A, B)): Node[A, B] = descendAndAdd(item, getPath(item._1.toString), this)

  def `++`: IterableOnce[(A, B)] => Node[A, B] = _.iterator.foldLeft(this)(_ + _)

  private def getPair(key: A, steps: List[Int], current: Node[A, B]): Option[(A, B)] =
    steps match {
      case head +: Nil => {
        val (position, isSet) = current.bitset.getPosition(head)
        if (!isSet) None
        else
          current.children(position) match {
            case Leaf(values) => values.find(_._1 == key)
            case _ => None // Cannot happen
          }
      }
      case head +: tail => {
        val (position, isSet) = current.bitset.getPosition(head)
        println((position, isSet))
        if (!isSet) None
        else
          current.children(position) match {
            case node: Node[A, B] => getPair(key, tail, node)
            case _ => None // Cannot happen
          }
      }
    }

  def get(key: A): Option[B] = getPair(key, getPath(key.toString), this).map(_._2)

  def has(key: A): Boolean = getPair(key, getPath(key.toString), this).isDefined
}

object HashArrayMappedTrie {

  def apply[A, B](initialItems: (A, B)*): Node[A, B] = Node() ++ initialItems
}

object HamtApp extends App {

  val hamt = HashArrayMappedTrie((0 -> "foo"), (5 -> "bar"), (32 -> "baz"), (512 -> "test"))
  assert(hamt.has(0))
  assert(hamt.has(5))
  assert(!hamt.has(6))
  assert(!hamt.has(31))
  assert(hamt.has(32))
  assert(hamt.has(512))
  assert(hamt.get(512).get == "test")
}
