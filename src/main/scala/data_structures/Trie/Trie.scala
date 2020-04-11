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
        val updatedChildren = trie.children.updated(position, updatedChild)
        trie.copy(children=updatedChildren)
      } else {
        val newBitset = trie.bitset + head
        val newChild = new Trie(isWord=true)
        val updatedChildren = trie.children.insertAt(position, newChild)
        trie.copy(bitset=newBitset, children=updatedChildren)
      }
    } else {
      if (isSet) {
        val updatedChild = insert(tail, trie.children(position))
        val updatedChildren = trie.children.updated(position, updatedChild)
        trie.copy(children=updatedChildren)
      } else {
        val newBitset = trie.bitset + head
        val newChild = insert(tail, new Trie)
        val updatedChildren = trie.children.insertAt(position, newChild)
        trie.copy(bitset=newBitset, children=updatedChildren)
      }
    }
  }

  def +(word: String): Trie = insert(getIndexesFromString(word), this)

  def ++(words: IterableOnce[String]): Trie = words.iterator.foldLeft(this)(_ + _)

  def ++(trie: Trie): Trie = this ++ trie.toList

  private def keys(currentPrefix: Seq[Int]): List[Seq[Int]] = {
    val words =
      (this.bitset.toList zip this.children)
        .flatMap({
          case (char, subTrie) => subTrie.keys(currentPrefix :+ char)
        })
    if (this.isWord) currentPrefix +: words
    else words
  }

  private def getNFirst(n: Int, prefix: Vector[Int], soFar: List[Vector[Int]] = List()): List[Vector[Int]] =
    if (soFar.size >= n) soFar
    else
      (this.bitset.toList zip this.children)
        .foldLeft(soFar)((acc, tpl) => {
          if (acc.size >= n) acc
          val (char, subTrie) = tpl
          val word = prefix :+ char
          if (subTrie.isWord)
            subTrie.getNFirst(n, word, acc :+ word)
          else
            subTrie.getNFirst(n, word, acc)
        })

  def getNBelow(n: Int, remainingChars: String, basePrefix: String): List[String] =
    if (remainingChars.isEmpty) {
      val base = if (this.isWord) List(basePrefix) else List.empty
      base ++ this.getNFirst(n, getIndexesFromString(basePrefix))
        .map(getStringFromIndexes)
    } else {
      val firstChar = remainingChars.head
      val (position, _) = this.bitset.getPosition(firstChar.toInt)
      this.children
        .lift(position)
        .fold(List[String]())(_.getNBelow(n, remainingChars.tail, basePrefix))
    }

  def toList: List[String] = keys(Vector.empty).map(getStringFromIndexes)
}

object Trie {

  def apply(initialItems: String*): Trie = new Trie ++ initialItems
}

object TrieApp extends App {

  val data = List(
    "project runway",
    "pinterest",
    "river",
    "kayak",
    "progenex",
    "progeria",
    "pg&e",
    "project free tv",
    "bank",
    "proactive",
    "progesterone",
    "press democrat",
    "priceline",
    "pandora",
    "reprobe",
    "paypal",
  )
  val trie = Trie(data:_*)
  println(trie.getNBelow(4, "p", "p"))
}
