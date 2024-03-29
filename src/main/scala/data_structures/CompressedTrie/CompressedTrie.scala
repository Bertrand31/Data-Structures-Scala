package data_structures

import cats.implicits._
import org.roaringbitmap.RoaringBitmap
import Utils.AugmentedArray
import TrieUtils.{getIndexesFromString, getStringFromIndexes}
import RoaringBitmapUtils.AugmentedBitmap

final case class CompressedTrie(
  private val bitset: RoaringBitmap = new RoaringBitmap,
  private val children: Array[CompressedTrie] = new Array(0),
  private val isWord: Boolean = false,
) {

  type WordPath = Vector[Int]

  private def insert(chars: Seq[Int]): CompressedTrie = {
    val head +: tail = chars
    val (position, isSet) = this.bitset.getPosition(head)
    if (tail.isEmpty)
      if (isSet) {
        val updatedChild = this.children(position).copy(isWord=true)
        val updatedChildren = this.children.updated(position, updatedChild)
        this.copy(children=updatedChildren)
      } else {
        val newBitset = this.bitset + head
        val newChild = new CompressedTrie(isWord=true)
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

  def add(str: String): CompressedTrie =
    insert(str.toCharArray.map(_.toInt).toSeq)

  def `+`: String => CompressedTrie = getIndexesFromString >>> this.insert

  def `++`: IterableOnce[String] => CompressedTrie = _.iterator.foldLeft(this)(_ + _)

  private def keys(currentPrefix: WordPath): List[WordPath] = {
    val words =
      (this.bitset.toIterator zip this.children)
        .flatMap({
          case (char, subTrie) => subTrie.keys(currentPrefix :+ char)
        })
        .toList
    if (this.isWord) currentPrefix +: words else words
  }

  private def getNFirst(n: Int, prefix: WordPath, soFar: List[WordPath] = List()): List[WordPath] =
    if (soFar.size >= n) soFar
    else
      (this.bitset.toIterator zip this.children)
        .foldLeft(soFar)((acc, tpl) =>
          if (acc.size >= n) acc
          else {
            val (char, subTrie) = tpl
            val word = prefix :+ char
            if (subTrie.isWord)
              subTrie.getNFirst(n, word, word +: acc)
            else
              subTrie.getNFirst(n, word, acc)
          }
        )

  private def getNBelow(n: Int, remainingChars: WordPath, basePrefix: String): List[String] =
    if (remainingChars.isEmpty) {
      val basePath = getIndexesFromString(basePrefix)
      val nFirstBelow = getNFirst(n, basePath).reverse.map(getStringFromIndexes)
      if (this.isWord) basePrefix +: nFirstBelow else nFirstBelow
    } else {
      val firstChar +: restChars = remainingChars
      val (position, isSet) = this.bitset.getPosition(firstChar)
      if (!isSet) List()
      else this.children(position).getNBelow(n, restChars, basePrefix)
    }

  def getNBelow(n: Int, prefix: String): List[String] =
    getNBelow(n, getIndexesFromString(prefix), prefix)

  def toList: List[String] = keys(Vector.empty).map(getStringFromIndexes)

  def isEmpty: Boolean = this.bitset.isEmpty
}

object CompressedTrie {

  def apply(initialItems: IterableOnce[String]): CompressedTrie =
    new CompressedTrie ++ initialItems

  def apply(initialItems: String*): CompressedTrie =
    CompressedTrie(initialItems)
}
