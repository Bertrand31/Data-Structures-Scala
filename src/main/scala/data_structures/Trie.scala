package data_structures

import cats.implicits._
import TrieUtils.{getIndexesFromString, getStringFromIndexes}

final case class Trie(
  private val children: Array[Option[Trie]],
  private val isFinal: Boolean,
) {

  type Path = Vector[Int]

  private def insertIndexes: Seq[Int] => Trie =
    _ match {
      case head +: Nil => {
        val newSubTrie = this.children(head).getOrElse(Trie()).copy(isFinal=true)
        this.copy(children=this.children.updated(head, Some(newSubTrie)))
      }
      case head +: tail => {
        val newSubTrie = this.children(head) match {
          case Some(subTrie) => subTrie.insertIndexes(tail)
          case None          => Trie().insertIndexes(tail)
        }
        this.copy(children=this.children.updated(head, Some(newSubTrie)))
      }
    }

  def `+`: String => Trie = getIndexesFromString >>> this.insertIndexes

  def `++`: IterableOnce[String] => Trie = _.iterator.foldLeft(this)(_ + _)

  def merge(trie: Trie): Trie = trie.keys.foldLeft(this)(_ + _)

  private def endsOnLastIndex: Seq[Int] => Boolean =
    _ match {
      case head +: Nil  => this.children(head).fold(false)(_.isFinal)
      case head +: tail => this.children(head).fold(false)(_.endsOnLastIndex(tail))
    }

  def contains: String => Boolean = getIndexesFromString >>> this.endsOnLastIndex

  private def descendCharByChar(chars: Path = Vector()): List[Path] =
    (0 until this.children.size).flatMap(index =>
      this.children(index) match {
        case None => Vector()
        case Some(subTrie) if (subTrie.isFinal) =>
          val currentWord = chars :+ index
          currentWord +: subTrie.descendCharByChar(currentWord)
        case Some(subTrie) => subTrie.descendCharByChar(chars :+ index)
      }
    ).toList

  def keys: List[String] = this.descendCharByChar().map(getStringFromIndexes)

  private def takeNWords(n: Int, chars: Path = Vector(), soFar: List[Path] = List()): List[Path] =
    (0 until this.children.size).foldLeft(soFar)((acc, index) =>
      if (acc.size >= n) acc
      else
        this.children(index) match {
          case Some(subTrie) if (subTrie.isFinal) =>
            val currentWord = chars :+ index
            val newSoFar = acc :+ currentWord
            if (newSoFar.size >= n) newSoFar
            else subTrie.takeNWords(n, currentWord, newSoFar)
          case Some(subTrie) => subTrie.takeNWords(n, chars :+ index, acc)
          case None => acc
        }
    )

  def firstNKeys: Int => List[String] = this.takeNWords(_).map(getStringFromIndexes)

  private def descendWithPrefix: Seq[Int] => Option[Trie] =
    _ match {
      case head +: Nil  => this.children(head)
      case head +: tail => this.children(head).flatMap(_.descendWithPrefix(tail))
    }

  def keysWithPrefix(prefix: String, max: Option[Int]): List[String] = {
    val subTrie = this.descendWithPrefix(getIndexesFromString(prefix))
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
    new Trie(
      children=Array.fill[Option[Trie]](AlphabetLength)(None),
      isFinal=false,
    ) ++ initialItems
}
