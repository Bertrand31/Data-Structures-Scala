package data_structures.hamt

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3.stringHash

sealed trait HashArrayMappedTrie[+A, +B]

final case class Leaf[A, B](values: Array[(A, B)]) extends HashArrayMappedTrie[A, B]

final case class Node[A: ClassTag, B](
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

  private def internalHas(key: A, steps: List[Int], current: Node[A, B]): Boolean =
    steps match {
      case head +: Nil => {
        val (position, isSet) = current.bitset.getPosition(head)
        if (!isSet) false
        else
          current.children(position) match {
            case Leaf(values) => values.exists(_._1 == key)
            case _ => false // Cannot happen
          }
      }
      case head +: tail => {
        val (position, isSet) = current.bitset.getPosition(head)
        println((position, isSet))
        if (!isSet) false
        else
          current.children(position) match {
            case node: Node[A, B] => internalHas(key, tail, node)
            case _ => false // Cannot happen
          }
      }
    }

  def has(key: A): Boolean = internalHas(key, getPath(key.toString), this)
}

object HashArrayMappedTrie {

  def apply[A: ClassTag, B](initialItems: (A, B)*): Node[A, B] = Node() ++ initialItems
}

object HamtApp extends App {

  val hamt = HashArrayMappedTrie((0 -> "foo"), (5 -> "bar"), (32 -> "baz"), (512 -> "test"))
  assert(hamt.has(0))
  assert(hamt.has(5))
  assert(!hamt.has(6))
  assert(!hamt.has(31))
  assert(hamt.has(32))
  assert(hamt.has(512))
}
