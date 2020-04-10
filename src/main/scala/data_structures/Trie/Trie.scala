package data_structures.trie

import org.roaringbitmap.RoaringBitmap
import data_structures.Utils.AugmentedArray
import TrieUtils.{getIndexesFromString, getStringFromIndexes}
import RoaringBitmapUtils.AugmentedBitmap

final case class Trie(
  private val bitset: RoaringBitmap = new RoaringBitmap,
  private val children: Array[Trie] = new Array(0),
  private val isWord: Boolean = false,
) {

  def insert(chars: Seq[Int], trie: Trie): Trie = {
    val head +: tail = chars
    val (position, isSet) = trie.bitset.getPosition(head)
    if (tail.isEmpty) {
      if (isSet) {
        val updatedChild = trie.children(position).copy(isWord=true)
        trie.copy(children=trie.children.updated(position, updatedChild))
      } else {
        val newBitset = trie.bitset + head
        val newChild = new Trie(isWord=true)
        trie.copy(bitset=newBitset, children=trie.children.insertAt(position, newChild))
      }
    } else {
      if (isSet) {
        val newChild = insert(tail, trie.children(position))
        trie.copy(children=children.updated(position, newChild))
      } else {
        val newBitset = trie.bitset + head
        val newChild = insert(tail, new Trie)
        trie.copy(bitset=newBitset, children=trie.children.insertAt(position, newChild))
      }
    }
  }

  def +(word: String): Trie = insert(getIndexesFromString(word), this)

  def ++(words: IterableOnce[String]): Trie = words.iterator.foldLeft(this)(_ + _)

  def ++(trie: Trie): Trie = this ++ trie.keys

  private def keys(currentPrefix: Seq[Int]): List[Seq[Int]] = {
    val chars = this.bitset.toList
    val words =
      (chars zip this.children)
        .flatMap({
          case (char, subTrie) => subTrie.keys(currentPrefix :+ char)
        })
    if (this.isWord) currentPrefix +: words
    else words
  }

  def keys: List[String] = keys(Seq.empty).map(getStringFromIndexes)
}

object Trie {

  def apply(initialItems: String*): Trie =
    new Trie ++ initialItems
}

object TrieApp extends App {

  val data = List(
    "pinterest",
    "river",
  )
  val trie = Trie(data:_*)
  println(trie.keys)
}
