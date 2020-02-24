package data_structures.hamt

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3.stringHash

sealed trait HashArrayMappedTrie[+A]

final case class Leaf[A: ClassTag](values: Array[A]) extends HashArrayMappedTrie[A]

final case class Node[A: ClassTag](
  bitset: Simple32BitSet = Simple32BitSet(),
  children: Array[HashArrayMappedTrie[A]] = new Array[HashArrayMappedTrie[A]](0),
) extends HashArrayMappedTrie[A] {

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

  private def descendAndAdd(item: A, steps: List[Int], current: Node[A]): Node[A] =
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
            case node: Node[A] => {
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

  def +(item: A): Node[A] = descendAndAdd(item, getPath(item.toString), this)

  def `++`: IterableOnce[A] => Node[A] = _.iterator.foldLeft(this)(_ + _)

  private def internalContains(elem: A, steps: List[Int], current: Node[A]): Boolean =
    steps match {
      case head +: Nil => {
        val (position, isSet) = current.bitset.getPosition(head)
        if (!isSet) false
        else
          current.children(position) match {
            case Leaf(values) => values.contains(elem)
            case _ => false // Cannot happen
          }
      }
      case head +: tail => {
        val (position, isSet) = current.bitset.getPosition(head)
        if (!isSet) false
        else
          current.children(position) match {
            case node: Node[A] => internalContains(elem, tail, node)
            case _ => false // Cannot happen
          }
      }
    }

  def contains(elem: A): Boolean = internalContains(elem, getPath(elem.toString), this)
}

object HashArrayMappedTrie {

  def apply[A: ClassTag](initialItems: A*): Node[A] = Node() ++ initialItems
}

object HamtApp extends App {

  val hamt = HashArrayMappedTrie(0, 5, 32, 512)
  assert(hamt.contains(0))
  assert(hamt.contains(5))
  assert(!hamt.contains(6))
  assert(!hamt.contains(31))
  assert(hamt.contains(32))
  assert(hamt.contains(512))
}
