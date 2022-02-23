package data_structures.hamt

import scala.collection.View
import scala.collection.immutable.ArraySeq
import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps
import cats.{Eq, Eval, Foldable, Functor, Monoid, Semigroup, Show}
import cats.implicits._
import data_structures.Utils._
import Simple32BitSetContainer.Simple32BitSet

sealed trait HashArrayMappedTrie[+A, +B] {

  def view: View[(A, B)]

  def keys: View[A]

  def values: View[B]
}

private final case class Leaf[A, B](
  private val bitset: Simple32BitSet = Simple32BitSet.empty,
  private val storedValues: ArraySeq[(A, B)] = ArraySeq.empty,
) extends HashArrayMappedTrie[A, B] {

  def view: View[(A, B)] = storedValues.view

  def keys: View[A] = view.map(_._1)

  def values: View[B] = view.map(_._2)

  def getValue(word: Int): Option[B] = {
    val (position, isSet) = this.bitset.getPosition(word)
    if (isSet) Some(this.storedValues(position)._2)
    else None
  }

  def add(word: Int, kv: (A, B)): Leaf[A, B] = {
    val position = this.bitset.countOnesUpTo(word)
    Leaf(
      bitset=this.bitset.add(word),
      storedValues=this.storedValues.insertAt(position, kv),
    )
  }

  def remove(word: Int): Leaf[A, B] = {
    val (position, isSet) = this.bitset.getPosition(word)
    if (!isSet) this
    else
      Leaf(
        bitset=this.bitset.remove(word),
        storedValues=this.storedValues.removeAt(position),
      )
  }

  def isEmpty: Boolean = this.bitset.isEmpty
}

final case class Node[A, B](
  private val bitset: Simple32BitSet = Simple32BitSet.empty,
  private val children: ArraySeq[HashArrayMappedTrie[A, B]] = ArraySeq.empty,
) extends HashArrayMappedTrie[A, B] {

  import Node._

  private def descendAndAdd(item: (A, B), steps: Iterator[Int], current: Node[A, B]): Node[A, B] = {
    val currentStep = steps.next()
    val (position, isSet) = current.bitset.getPosition(currentStep)
    if (!steps.hasNext) {
      val newLeaf = Leaf[A, B]().add(currentStep, item)
      val newChildren =
        if (position >= current.children.size) current.children :+ newLeaf
        else current.children.updated(position, newLeaf)
      val newBitset = current.bitset.add(currentStep)
      current.copy(bitset=newBitset, children=newChildren)
    } else
      if (isSet) {
        val newChildren = current.children(position) match {
          case _: Leaf[A, B] => current.children // Impossible
          case node: Node[A, B] =>
            val newChild = descendAndAdd(item, steps, node)
            current.children.updated(position, newChild)
        }
        current.copy(children=newChildren)
      } else {
        val newChild = descendAndAdd(item, steps, Node())
        val newChildren = current.children.insertAt(position, newChild)
        val newBitSet = current.bitset.add(currentStep)
        current.copy(children=newChildren, bitset=newBitSet)
      }
  }

  def +(item: (A, B)): Node[A, B] = descendAndAdd(item, getPath(item._1), this)

  val `++`: IterableOnce[(A, B)] => Node[A, B] = _.iterator.foldLeft(this)(_ + _)

  private def descendAndRemove(key: A, steps: Iterator[Int], current: Node[A, B]): Node[A, B] = {
    val currentStep = steps.next()
    val (position, isSet) = current.bitset.getPosition(currentStep)
    if (isSet)
      current.children(position) match {
        case _: Leaf[A, B] =>
          val newChildren = current.children.removeAt(position)
          val newBitset = current.bitset.remove(currentStep)
          current.copy(children=newChildren, bitset=newBitset)
        case node: Node[A, B] =>
          val newChild = descendAndRemove(key, steps, node)
          val newChildren =
            if (newChild.bitset.isEmpty) current.children.removeAt(position)
            else current.children.updated(position, newChild)
          val newBitset = current.bitset.remove(currentStep)
          current.copy(children=newChildren, bitset=newBitset)
      }
    else current
  }

  def -(key: A): Node[A, B] = descendAndRemove(key, getPath(key), this)

  val `--`: IterableOnce[A] => Node[A, B] = _.iterator.foldLeft(this)(_ - _)

  @tailrec
  private def getPair(key: A, steps: Iterator[Int], current: Node[A, B]): Option[(A, B)] = {
    val currentStep = steps.next()
    val (position, isSet) = current.bitset.getPosition(currentStep)
    if (isSet)
      current.children(position) match {
        case leaf: Leaf[A, B] => leaf.getValue(currentStep).map((key, _))
        case node: Node[A, B] => getPair(key, steps, node)
      }
    else None
  }

  def get(key: A): Option[B] =
    getPair(key, getPath(key), this).map(_._2)

  def getOrElse(key: A, default: => B): B =
    get(key).getOrElse(default)

  def has(key: A): Boolean = get(key).isDefined

  def size: Int =
    this.children.foldLeft(0)({
      case (acc, node: Node[A, B]) => acc + node.size
      case (acc, Leaf(_, values))  => acc + values.size
    })

  def view: View[(A, B)] = this.children.view.flatMap(_.view)

  def keys: View[A] = this.children.view.flatMap(_.keys)

  def values: View[B] = this.children.view.flatMap(_.values)

  def toArray: Array[(A, B)] = this.view.toArray

  def foreach(fn: ((A, B)) => Unit): Unit = this.view.foreach(fn)

  type Predicate[T] = T => Boolean

  def forall(predicate: Predicate[(A, B)]): Boolean = this.view.forall(predicate)

  def count(predicate: Predicate[(A, B)]): Int = this.view.count(predicate)

  def countValues(predicate: Predicate[B]): Int = this.values.count(predicate)

  def find(predicate: Predicate[(A, B)]): Option[(A, B)] = this.view.find(predicate)

  def findValue(predicate: Predicate[B]): Option[B] = this.values.find(predicate)

  def isEmpty: Boolean = this.bitset.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def isTraversableAgain: Boolean = true
}

object Node {

  private val StepBits = 5
  // A number whose binary representation is `StepBits` 1s, to be used as a complement
  private val StepComplement = math.pow(2, StepBits).toInt - 1
  private val TrieDepth = math.ceil(Int.MaxValue.toBinaryString.size / StepBits.toDouble).toInt

  def makePathFromHash(hash: Int): Iterator[Int] =
    (0 until TrieDepth)
      .iterator
      .map(step => hash >> (StepBits * step) & StepComplement)

  def getPath(obj: Any): Iterator[Int] = makePathFromHash(obj.hashCode)

  implicit def nodeSemigroup[A, B] = new Semigroup[Node[A, B]] {

    def combine(x: Node[A, B], y: Node[A, B]): Node[A, B] =
      x ++ y.view
  }

  implicit def nodeMonoid[A, B] = new Monoid[Node[A, B]] {

    def empty: Node[A, B] = Node()

    def combine(x: Node[A, B], y: Node[A, B]): Node[A, B] =
      nodeSemigroup.combine(x, y)
  }

  type NodeT[T] = Node[_, T]

  implicit def nodeFunctor = new Functor[NodeT] {

    def map[A, B](fa: NodeT[A])(f: A => B): NodeT[B] =
      fa
        .view
        .map({ case (k, v) => (k, f(v)) })
        .iterator
        .pipe(HashArrayMappedTrie(_))
  }

  implicit def nodeShow[A, B] = new Show[HashArrayMappedTrie[A, B]] {

    def show(t: HashArrayMappedTrie[A,B]): String =
      "HashArrayMappedTrie(" ++ t.view.map({ case (k, v) => s"$k -> $v" }).mkString(", ") ++ ")"
  }

  implicit def nodeEq[A, B] = new Eq[HashArrayMappedTrie[A, B]] {

    def eqv(x: HashArrayMappedTrie[A,B], y: HashArrayMappedTrie[A,B]): Boolean =
      x.view.toMap == y.view.toMap
  }

  implicit def nodeFoldable = new Foldable[NodeT] {

    def foldLeft[A, B](fa: NodeT[A], b: B)(f: (B, A) => B): B =
      fa.view.foldLeft(b)({ case (acc, (_, v)) => f(acc, v) })

    def foldRight[A, B](fa: NodeT[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.view.foldRight(lb)({ case ((_, v), acc) => f(v, acc) })
  }
}

object HashArrayMappedTrie {

  def empty[A, B]: Node[A, B] = Node()

  def apply[A, B](initialItems: IterableOnce[(A, B)]): Node[A, B] =
    HashArrayMappedTrie.empty[A, B] ++ initialItems

  def apply[A, B](initialItems: (A, B)*): Node[A, B] =
    HashArrayMappedTrie(initialItems)
}
