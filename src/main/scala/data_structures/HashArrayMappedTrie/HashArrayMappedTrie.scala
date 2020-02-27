package data_structures.hamt

import scala.collection.View
import scala.reflect.ClassTag
import cats.implicits._
import ArrayUtils._

sealed trait HashArrayMappedTrie[+A, +B]

final case class Leaf[A: ClassTag, B: ClassTag](
  private val values: Array[(A, B)],
) extends HashArrayMappedTrie[A, B] {

  private def hasKey(key: A): ((A, B)) => Boolean = _._1 == key

  def +(item: (A, B)): Leaf[A, B] = Leaf(this.values :+ item)

  def -(key: A): Leaf[A, B] = Leaf(this.values.filterNot(hasKey(key)))

  def findKey(key: A): Option[(A, B)] = this.values.find(hasKey(key))

  def isEmpty: Boolean = this.values.isEmpty
}

final case class Node[A: ClassTag, B: ClassTag](
  private val bitset: Simple32BitSet = Simple32BitSet(),
  private val children: Array[HashArrayMappedTrie[A, B]] = new Array[HashArrayMappedTrie[A, B]](0),
) extends HashArrayMappedTrie[A, B] {

  private val StepBits  = 5
  private val TrieDepth = Math.ceil(Int.MaxValue.toBinaryString.length / StepBits).toInt

  private def makePathFromHash(hash: Int): Iterator[Int] =
    (0 until TrieDepth)
      .iterator
      .map(chunkNumber => {
        val number = hash >> (StepBits * chunkNumber)
        val newStepBinary =
          (0 until StepBits)
            .map(shift => if ((number & ~(1L << shift)) =!= number) 1 else 0)
            .mkString
        Integer.parseInt(newStepBinary, 2)
      })

  private def getPath(obj: Any): Iterator[Int] = makePathFromHash(obj.hashCode)

  private def descendAndAdd(item: (A, B), steps: Iterator[Int], current: Node[A, B]): Node[A, B] = {
    val currentStep = steps.next
    val (position, isSet) = current.bitset.getPosition(currentStep)
    if (isSet) {
      val newChildren = current.children(position) match {
        case leaf: Leaf[A, B] => current.children.updated(position, leaf + item)
        case node: Node[A, B] => current.children.updated(position, descendAndAdd(item, steps, node))
      }
      current.copy(children=newChildren)
    } else
      if (!steps.hasNext) {
        val newChildren = current.children.insertAt(position, Leaf(item))
        val newBitSet = current.bitset + currentStep
        current.copy(children=newChildren, bitset=newBitSet)
      } else {
        val newChild = descendAndAdd(item, steps, Node())
        val newChildren = current.children.insertAt(position, newChild)
        val newBitSet = current.bitset + currentStep
        current.copy(children=newChildren, bitset=newBitSet)
      }
  }

  def +(item: (A, B)): Node[A, B] = descendAndAdd(item, getPath(item._1), this)

  def `++`: IterableOnce[(A, B)] => Node[A, B] = _.iterator.foldLeft(this)(_ + _)

  private def descendAndRemove(key: A, steps: Iterator[Int], current: Node[A, B]): Node[A, B] = {
    val currentStep = steps.next
    val (position, isSet) = current.bitset.getPosition(currentStep)
    if (isSet) {
      current.children(position) match {
        case leaf: Leaf[A, B] =>
          val newLeaf = leaf - key
          if (newLeaf.isEmpty) {
            val newChildren = current.children.removeAt(position)
            val newBitset = current.bitset - currentStep
            current.copy(children=newChildren, bitset=newBitset)
          } else {
            val newChildren = current.children.updated(position, newLeaf)
            current.copy(children=newChildren)
          }
        case node: Node[A, B] =>
          val newChild = descendAndRemove(key, steps, node)
          val newChildren =
            if (newChild.bitset.isEmpty) current.children.removeAt(position)
            else current.children.updated(position, newChild)
          val newBitset = current.bitset - currentStep
          current.copy(children=newChildren, bitset=newBitset)
      }
    } else current
  }

  def -(key: A): Node[A, B] = descendAndRemove(key, getPath(key), this)

  def `--`: IterableOnce[A] => Node[A, B] = _.iterator.foldLeft(this)(_ - _)

  private def getPair(key: A, steps: Iterator[Int], current: Node[A, B]): Option[(A, B)] = {
    val currentStep = steps.next
    val (position, isSet) = current.bitset.getPosition(currentStep)
    if (isSet)
      current.children(position) match {
        case leaf: Leaf[A, B] => leaf.findKey(key)
        case node: Node[A, B] => getPair(key, steps, node)
      }
    else None
  }

  def get(key: A): Option[B] =
    getPair(key, getPath(key), this).map(_._2)

  def getOrElse(key: A, default: => B): B =
    getPair(key, getPath(key), this).fold(default)(_._2)

  def has(key: A): Boolean = getPair(key, getPath(key), this).isDefined

  def size: Int =
    this.children.foldLeft(0)({
      case (acc, node: Node[A, B]) => acc + node.size
      case (acc, Leaf(values)) => acc + values.size
    })

  def view: View[(A, B)] =
    this.children.view.flatMap({
      case node: Node[A, B] => node.view
      case Leaf(values) => values
    })

  def toArray: Array[(A, B)] = this.view.toArray

  def map[C: ClassTag, D: ClassTag](fn: ((A, B)) => (C, D)): Node[C, D] =
    HashArrayMappedTrie(this.view.map(fn))

  def foreach(fn: ((A, B)) => Unit): Unit = this.view.foreach(fn)

  def keys: View[A] = this.view.map(_._1)

  def values: View[B] = this.view.map(_._2)

  def count(predicate: ((A, B)) => Boolean): Int = this.view.count(predicate)

  def countValues(predicate: B => Boolean): Int = this.values.count(predicate)

  def find(predicate: ((A, B)) => Boolean): Option[(A, B)] = this.view.find(predicate)

  def findValue(predicate: B => Boolean): Option[B] = this.values.find(predicate)

  def isEmpty: Boolean = this.bitset.isEmpty
}

object HashArrayMappedTrie {

  def apply[A: ClassTag, B: ClassTag](initialItems: IterableOnce[(A, B)]): Node[A, B] =
    Node[A, B]() ++ initialItems

  def apply[A: ClassTag, B: ClassTag](initialItems: (A, B)*): Node[A, B] =
    HashArrayMappedTrie(initialItems)
}

object Leaf {

  def apply[A: ClassTag, B: ClassTag](items: (A, B)*): Leaf[A, B] = Leaf(Array(items:_*))
}
