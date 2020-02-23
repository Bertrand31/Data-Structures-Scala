package data_structures.hamt

import scala.util.hashing.MurmurHash3.stringHash

sealed trait HashArrayMappedTrie[+A]

final case class Leaf[A](values: IndexedSeq[A]) extends HashArrayMappedTrie[A]

final case class Node[A](
  bitSet: Simple32BitSet = Simple32BitSet(),
  children: IndexedSeq[HashArrayMappedTrie[A]] = IndexedSeq[HashArrayMappedTrie[A]](),
) extends HashArrayMappedTrie[A] {

  private val StepBits = 5

  private def getPath(str: String): List[Int] =
    stringHash(str)
      .toBinaryString
      .reverse
      .toSeq
      .sliding(StepBits, StepBits)
      .map(_.unwrap)
      .map(Integer.parseInt(_, 2))
      .toList

  def +(item: A): Node[A] = {
    val path = getPath(item.toString)

    def descendAndAdd(steps: List[Int], current: Node[A]): Node[A] =
      steps match {
        case head +: Nil => {
          val (position, isSet) = current.bitSet.getPosition(head)
          if (isSet) {
            val newChildren = current.children(position) match {
              case Leaf(values) => current.children.updated(position, Leaf(values :+ item))
              case _ => current.children.updated(position, Leaf(IndexedSeq(item)))
            }
            current.copy(children=newChildren)
          } else {
            val newChildren = (current.children.take(position - 1) :+ Leaf(IndexedSeq(item))) ++ current.children.drop(position)
            val newBitSet = current.bitSet + head
            current.copy(children=newChildren, bitSet=newBitSet)
          }
        }
        case head +: tail => {
          val (position, isSet) = current.bitSet.getPosition(head)
          if (isSet) {
            current.children(position) match {
              case node: Node[A] => {
                val newChild = descendAndAdd(tail, node)
                val newChildren = current.children.updated(position, newChild)
                current.copy(children=newChildren)
              }
            }
          } else {
            val newChild = descendAndAdd(tail, Node())
            val newChildren = (current.children.take(position - 1) :+ newChild) ++ current.children.drop(position)
            val newBitSet = current.bitSet + head
            current.copy(children=newChildren, bitSet=newBitSet)
          }
        }
      }

   descendAndAdd(path, this)
  }

  private def internalContains(elem: A, steps: List[Int], current: Node[A]): Boolean =
    steps match {
      case head +: Nil => {
        val (position, isSet) = current.bitSet.getPosition(head)
        if (!isSet) false
        else {
          current.children(position) match {
            case Leaf(values) => values.contains(elem)
            case _ => false
          }
        }
      }
      case head +: tail => {
        val (position, isSet) = current.bitSet.getPosition(head)
        if (!isSet) false
        else {
          current.children(position) match {
            case node: Node[A] => internalContains(elem, tail, node)
            case _ => false
          }
        }
      }
    }

  def contains(elem: A): Boolean = internalContains(elem, getPath(elem.toString), this)

  def `++`: IterableOnce[A] => Node[A] = _.iterator.foldLeft(this)(_ + _)
}

object HashArrayMappedTrie {

  def apply[A](initialItems: A*): HashArrayMappedTrie[A] = Node() ++ initialItems
}

object HamtApp extends App {

  val hamt = Node[Int]()
  val hamtWithD = hamt + 5
  println(hamtWithD)
  println(hamtWithD contains 5)
  println(hamtWithD contains 6)
}
