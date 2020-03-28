// THIS IS A SCRAPPY WORK-IN-PROGRESS

package data_structures

import cats.implicits._

final case class RadixTree(
  private val children: Map[Char, RadixTree] = Map.empty,
  private val chunk: List[Char] = List.empty,
  private val isWord: Boolean = false,
) {

  def add(chars: List[Char]): RadixTree =
    chars match {
      case Nil => new RadixTree(isWord=true)
      case head +: Nil =>
        this.children.get(head) match {
          case None => {
            val newChild = new RadixTree(isWord=true)
            val newChildren = this.children + (head -> newChild)
            this.copy(children=newChildren)
          }
          case Some(subTree) =>
            val newChild =
              if (subTree.chunk.isEmpty) subTree.copy(isWord=true)
              else {
                val newSub =
                  new RadixTree(
                    isWord=subTree.isWord,
                    chunk=subTree.chunk.tail,
                    children=subTree.children,
                  )
                new RadixTree(
                  isWord=true,
                  chunk=List.empty,
                  children=(Map() + (subTree.chunk.head -> newSub)),
                )
              }
            val newChildren = this.children + (head -> newChild)
            this.copy(children=newChildren)
        }
      case head +: tail => {
        val newChild = this.children.get(head) match {
          case None => new RadixTree(chunk=tail, isWord=true)
          case Some(subTree) => {
            val commonPrefixLength =
              (tail zip subTree.chunk).segmentLength({ case (a, b) => a === b })

            val (commonPrefix, rest) = tail.splitAt(commonPrefixLength)
            val restHead +: restTail = rest
            val newChild =
              subTree.children
                .get(restHead)
                .fold(new RadixTree(chunk=restTail, isWord=true))(_.add(restTail))

            val oldRest = subTree.chunk.drop(commonPrefixLength)
            if (oldRest.isEmpty) subTree.copy(isWord=true)
            else {
              val oldRestHead +: oldRestTail = oldRest
              val newK = subTree.copy(children=subTree.children, chunk=oldRestTail)

              val newChildren = Map.empty + (restHead -> newChild) + (oldRestHead -> newK)
              new RadixTree(children=newChildren, chunk=commonPrefix)
            }
          }
        }
        this.copy(children=(this.children + (head -> newChild)))
      }
    }

  def +(word: String): RadixTree = add(word.toList)

  def `++`: IterableOnce[String] => RadixTree = _.iterator.foldLeft(this)(_ + _)

  def toList(soFar: String): List[String] =
    this.children
      .flatMap({
        case (char, subTree) =>
          val current =  (soFar :+ char) ++ subTree.chunk.mkString
          val wordsBelow = subTree.toList(current)
          if (subTree.isWord) current +: wordsBelow else wordsBelow
      })
      .toList

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

  val tree = RadixTree() ++ List("trumped", "trumpet", "trumpistan", "t", "tru")
  println(tree)
  assert(!tree.contains("trum"))
  assert(tree.contains("trumped"))
  assert(!tree.contains("trumpt"))
  assert(!tree.contains("trumpd"))
  assert(tree.contains("trumpet"))
  assert(!tree.contains("trumpis"))
  assert(tree.contains("trumpistan"))
  assert(tree.contains("t"))
  assert(tree.contains("tru"))
}
