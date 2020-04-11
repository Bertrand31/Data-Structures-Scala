package data_structures

import cats.implicits._
import org.roaringbitmap.RoaringBitmap
import Utils.AugmentedArray
import TrieUtils.{getIndexesFromString, getStringFromIndexes}
import RoaringBitmapUtils.AugmentedBitmap

final case class CompressedTrie(
  private val bitset: RoaringBitmap = new RoaringBitmap,
  private val children: Array[CompressedTrie] = new Array(0),
  private val isPath: Boolean = false,
) {

  type Path = Vector[Int]

  def insert(chars: Seq[Int]): CompressedTrie = {
    val head +: tail = chars
    val (position, isSet) = this.bitset.getPosition(head)
    if (tail.isEmpty)
      if (isSet) {
        val updatedChild = this.children(position).copy(isPath=true)
        val updatedChildren = this.children.updated(position, updatedChild)
        this.copy(children=updatedChildren)
      } else {
        val newBitset = this.bitset + head
        val newChild = new CompressedTrie(isPath=true)
        val updatedChildren = this.children.insertAt(position, newChild)
        this.copy(bitset=newBitset, children=updatedChildren)
      }
    else
      if (isSet) {
        val updatedChild = this.children(position).insert(tail)
        val updatedChildren = this.children.updated(position, updatedChild)
        this.copy(children=updatedChildren)
      } else {
        val newBitset = this.bitset + head
        val newChild = CompressedTrie().insert(tail)
        val updatedChildren = this.children.insertAt(position, newChild)
        this.copy(bitset=newBitset, children=updatedChildren)
      }
  }

  def `+`: String => CompressedTrie = getIndexesFromString >>> this.insert

  def `++`: IterableOnce[String] => CompressedTrie = _.iterator.foldLeft(this)(_ + _)

  private def keys(currentPrefix: Path): List[Path] = {
    val words =
      (this.bitset.toList zip this.children)
        .flatMap({
          case (char, subTrie) => subTrie.keys(currentPrefix :+ char)
        })
    if (this.isPath) currentPrefix +: words else words
  }

  private def getNFirst(n: Int, prefix: Path, soFar: List[Path] = List()): List[Path] =
    if (soFar.size >= n) soFar
    else
      (this.bitset.toList zip this.children)
        .foldLeft(soFar)((acc, tpl) => {
          if (acc.size >= n) acc
          else {
            val (char, subTrie) = tpl
            val word = prefix :+ char
            if (subTrie.isPath)
              subTrie.getNFirst(n, word, word +: acc)
            else
              subTrie.getNFirst(n, word, acc)
          }
        })

  def getNBelow(n: Int, remainingChars: String, basePrefix: String): List[String] =
    if (remainingChars.isEmpty) {
      val basePath = getIndexesFromString(basePrefix)
      val nFirstBelow = getNFirst(n, basePath) map getStringFromIndexes
      if (this.isPath) basePrefix +: nFirstBelow else nFirstBelow
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

  def apply(initialItems: IterableOnce[String]): CompressedTrie =
    new CompressedTrie ++ initialItems

  def apply(initialItems: String*): CompressedTrie =
    CompressedTrie(initialItems)
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
