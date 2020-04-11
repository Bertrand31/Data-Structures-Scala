package data_structures

import cats.implicits._
import TrieUtils.{getIndexesFromString, getStringFromIndexes}

final case class Trie(
  private val children: Array[Option[Trie]],
  private val isFinal: Boolean,
) {

  private def insertIndexes: Seq[Int] => Trie =
    _ match {
      case head +: Nil => {
        val newSubTrie = this.children(head).getOrElse(Trie()).copy(isFinal=true)
        this.copy(children=this.children.updated(head, Some(newSubTrie)))
      }
      case head +: tail => {
        val newSubTrie = this.children(head) match {
          case Some(subTrie) => subTrie.insertIndexes(tail)
          case None => Trie().insertIndexes(tail)
        }
        this.copy(children=this.children.updated(head, Some(newSubTrie)))
      }
    }

  def `+`: String => Trie = getIndexesFromString >>> this.insertIndexes

  def `++`: IterableOnce[String] => Trie = _.iterator.foldLeft(this)(_ + _)

  private def endsOnLastIndex: Seq[Int] => Boolean =
    _ match {
      case head +: Nil => this.children(head).fold(false)(_.isFinal)
      case head +: tail => this.children(head).fold(false)(_.endsOnLastIndex(tail))
    }

  def contains: String => Boolean = getIndexesFromString >>> this.endsOnLastIndex

  def keys(): List[String] = {
    def descendCharByChar(accumulator: Vector[Int], trie: Trie): List[Vector[Int]] =
      (0 to (trie.children.length - 1)).flatMap(index => {
        trie.children(index) match {
          case None => Vector()
          case Some(subTrie) if (subTrie.isFinal) => {
            val currentWord = accumulator :+ index
            currentWord +: descendCharByChar(currentWord, subTrie)
          }
          case Some(subTrie) => descendCharByChar(accumulator :+ index, subTrie)
        }
      }).toList

    descendCharByChar(Vector(), this).map(getStringFromIndexes)
  }

  def firstNKeys(n: Int): List[String] = {
    def takeNWords(accumulator: Vector[Int], trie: Trie, soFar: List[Vector[Int]]): List[Vector[Int]] =
      (0 to (trie.children.size - 1)).foldLeft(soFar)((acc, index) => {
        if (acc.size >= n) acc
        else
          trie.children(index) match {
            case Some(subTrie) if (subTrie.isFinal) =>
              val currentWord = accumulator :+ index
              val newSoFar = acc :+ currentWord
              if (newSoFar.size >= n) newSoFar
              else takeNWords(currentWord, subTrie, newSoFar)
            case Some(subTrie) =>
              takeNWords(accumulator :+ index, subTrie, acc)
            case None => acc
          }
      })

    takeNWords(Vector(), this, List.empty).map(getStringFromIndexes)
  }

  def keysWithPrefix(prefix: String, max: Option[Int]): List[String] = {

    def descendWithPrefix(indexes: Seq[Int], trie: Trie): Option[Trie] =
      indexes match {
        case head +: Nil => trie.children(head)
        case head +: tail => trie.children(head).flatMap(descendWithPrefix(tail, _))
      }

    val subTrie = descendWithPrefix(getIndexesFromString(prefix), this)
    subTrie match {
      case None => List()
      case Some(subTrie) if (subTrie.isFinal) =>
        max match {
          case Some(maxWords) => prefix +: subTrie.firstNKeys(maxWords - 1).map(prefix + _)
          case None => prefix +: subTrie.keys.map(prefix + _)
        }
      case Some(subTrie) =>
        max match {
          case Some(maxWords) => subTrie.firstNKeys(maxWords).map(prefix + _)
          case None => subTrie.keys.map(prefix + _)
        }
    }
  }

  def isEmpty: Boolean = this.children.forall(_.isEmpty)
}

object Trie {

  private val AlphabetLength = 122

  def apply(initialItems: String*): Trie =
    Trie(
      children=Array.fill[Option[Trie]](AlphabetLength)(None),
      isFinal=false,
    ) ++ initialItems
}
