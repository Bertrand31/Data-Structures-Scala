package data_structures

import cats.implicits._

final case class RadixTree(
  private val children: Map[Char, RadixTree] = Map.empty,
  private val chunk: List[Char] = List.empty,
  private val isWord: Boolean = false,
) {

  def add(chars: List[Char]): RadixTree =
    chars match {
      case Nil => this.copy(isWord=true)
      case head +: tail => {
        val newChild = this.children.get(head) match {
          case None => new RadixTree(chunk=tail, isWord=true)
          case Some(subTree) => {
            val commonPrefixLength = (tail zip subTree.chunk).segmentLength({ case (a, b) => a === b })
            val (commonPrefix, rest) = tail.splitAt(commonPrefixLength)
            val (_, oldRest) = subTree.chunk.splitAt(commonPrefixLength)
            val newChildNewWord = subTree.children.get(rest.head) match {
              case None => (rest.head -> new RadixTree(chunk=rest.tail, isWord=true))
              case Some(restTree) => (rest.head -> restTree.add(rest.tail))
            }
            val newChildOldWord = subTree.children.get(oldRest.head) match {
              case None => (oldRest.head -> new RadixTree(chunk=oldRest.tail, isWord=true))
              case Some(restTree) => (oldRest.head -> restTree.add(oldRest.tail))
            }
            val newChildren = subTree.children + newChildNewWord + newChildOldWord // FIXME: destructive
            subTree.copy(children=newChildren, chunk=commonPrefix)
          }
        }
        val newChildren = this.children + (head -> newChild)
        this.copy(children=newChildren)
      }
      case head +: Nil =>
        this.children.get(head) match {
          case None => {
            val newChild = new RadixTree(isWord=true)
            val newChildren = this.children + (head -> newChild)
            this.copy(children=newChildren)
          }
          case Some(subTree) =>
            if (subTree.chunk.isEmpty) subTree.copy(isWord=true)
            else subTree.copy(isWord=true).add(subTree.chunk)
        }
    }

  def +(word: String): RadixTree = add(word.toList)

  def `++`: IterableOnce[String] => RadixTree = _.iterator.foldLeft(this)(_ + _)

  def contains(chars: List[Char]): Boolean =
    chars match {
      case head +: Nil =>
        this.children.get(head).fold(false)(_.isWord)
      case head +: tail =>
        this.children.get(head)
          .map(subTree => {
            val (localChunk, rest) = tail.splitAt(subTree.chunk.size)
            if (rest.isEmpty) localChunk === subTree.chunk && subTree.isWord
            else {
              val localChunkMatches =
                subTree.chunk.zip(localChunk).forall({ case (a, b) => a === b })
              localChunkMatches && subTree.contains(rest)
            }
          })
          .getOrElse(false)
    }

  def contains(str: String): Boolean = this.contains(str.toList)
}

object RadixTree {

  def apply: IterableOnce[String] => RadixTree = new RadixTree ++ _

  def apply(initialItems: String*): RadixTree = apply(initialItems)
}

object RadixTreeTest extends App {

  val tree = RadixTree() ++ List("trumped", "trumpet", "trumpistan")
  println(tree)
  assert(!tree.contains("trum"))
  assert(tree.contains("trumped"))
  assert(tree.contains("trumpet"))
  assert(!tree.contains("trumpis"))
  assert(tree.contains("trumpistan"))
}
