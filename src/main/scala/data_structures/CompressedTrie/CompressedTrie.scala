package data_structures

import org.roaringbitmap.RoaringBitmap
import Utils.AugmentedArray
import TrieUtils.{getIndexesFromString, getStringFromIndexes}
import RoaringBitmapUtils.AugmentedBitmap

final case class CompressedTrie(
  private val bitset: RoaringBitmap = new RoaringBitmap,
  private val children: Array[CompressedTrie] = new Array(0),
  private val isWord: Boolean = false,
) {

  def insert(chars: Seq[Int], trie: CompressedTrie): CompressedTrie = {
    val head +: tail = chars
    val (position, isSet) = trie.bitset.getPosition(head)
    if (tail.isEmpty)
      if (isSet) {
        val updatedChild = trie.children(position).copy(isWord=true)
        val updatedChildren = trie.children.updated(position, updatedChild)
        trie.copy(children=updatedChildren)
      } else {
        val newBitset = trie.bitset + head
        val newChild = new CompressedTrie(isWord=true)
        val updatedChildren = trie.children.insertAt(position, newChild)
        trie.copy(bitset=newBitset, children=updatedChildren)
      }
    else
      if (isSet) {
        val updatedChild = insert(tail, trie.children(position))
        val updatedChildren = trie.children.updated(position, updatedChild)
        trie.copy(children=updatedChildren)
      } else {
        val newBitset = trie.bitset + head
        val newChild = insert(tail, new CompressedTrie)
        val updatedChildren = trie.children.insertAt(position, newChild)
        trie.copy(bitset=newBitset, children=updatedChildren)
      }
  }

  def +(word: String): CompressedTrie = insert(getIndexesFromString(word), this)

  def ++(words: IterableOnce[String]): CompressedTrie = words.iterator.foldLeft(this)(_ + _)

  def ++(trie: CompressedTrie): CompressedTrie = this ++ trie.toList

  private def keys(currentPrefix: Vector[Int]): List[Vector[Int]] = {
    val words =
      (this.bitset.toList zip this.children)
        .flatMap({
          case (char, subTrie) => subTrie.keys(currentPrefix :+ char)
        })
    if (this.isWord) currentPrefix +: words else words
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
            subTrie.getNFirst(n, word, word +: acc)
          else
            subTrie.getNFirst(n, word, acc)
        })

  def getNBelow(n: Int, remainingChars: String, basePrefix: String): List[String] =
    if (remainingChars.isEmpty) {
      val baseWord = getIndexesFromString(basePrefix)
      val nFirstBelow = getNFirst(n, baseWord) map getStringFromIndexes
      if (this.isWord) basePrefix +: nFirstBelow else nFirstBelow
    } else {
      val firstChar = remainingChars.head
      val (position, _) = this.bitset.getPosition(firstChar.toInt)
      this.children
        .lift(position)
        .fold(List[String]())(_.getNBelow(n, remainingChars.tail, basePrefix).reverse)
    }

  def toList: List[String] = keys(Vector.empty).map(getStringFromIndexes)

  def isEmpty: Boolean = this.bitset.isEmpty
}

object CompressedTrie {

  def apply(initialItems: String*): CompressedTrie = new CompressedTrie ++ initialItems
}

object CompressedTrieApp extends App {

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
  var cTrie = CompressedTrie()
  var trie = Trie()

  PerfUtils.profile("cTrie.apply") {
    cTrie = CompressedTrie(data:_*)
  }
  PerfUtils.profile("trie.apply") {
    trie = Trie(data:_*)
  }
  PerfUtils.profile("cTrie.getNBelow") {
    cTrie.getNBelow(4, "p", "p")
  }
  PerfUtils.profile("trie.keysWithPrefix") {
    trie.keysWithPrefix("p", Some(4))
  }
}
