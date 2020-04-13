package data_structures

import data_structures.BitSetContainer.{BitSet, BitSetBuilder}
import BitSet._

final case class Aurkitu(
  private val words: Map[Int, String]               = Map.empty,
  private val characterPositions: Map[Char, BitSet] = Map.empty,
  private val index: Map[(Char, Int), BitSet]       = Map.empty,
) {

  def +(word: String): Aurkitu = {
    val wordId = this.words.size
    val newWords = words + (wordId -> word)
    val pairs = word.zipWithIndex
    val newCharacterPositions = pairs.foldLeft(this.characterPositions)((acc, pair) => {
      val (char, charPosition) = pair
      val newBitSet = acc.getOrElse(char, BitSetBuilder()) + charPosition
      acc.updated(char, newBitSet)
    })
    val newIndex = pairs.foldLeft(this.index)((acc, pair) => {
      val newBitSet = acc.getOrElse(pair, BitSetBuilder()) + wordId
      acc.updated(pair, newBitSet)
    })
    Aurkitu(newWords, newCharacterPositions, newIndex)
  }

  def `++`: IterableOnce[String] => Aurkitu = _.iterator.foldLeft(this)(_ + _)

  def searchChunk: String => Set[String] =
    _
      .map(char =>
        characterPositions.get(char) match {
          case None => Set[String]()
          case Some(positions) =>
            positions
              .iterator
              .map((char, _))
              .flatMap(this.index.get)
              .flatMap(_.iterator)
              .map(this.words)
              .toSet
        }
      )
      .reduce(_ intersect _)

  def toList: Iterable[String] = this.words.values
}

object Aurkitu {

  def apply(initialItems: IterableOnce[String]): Aurkitu =
    new Aurkitu ++ initialItems

  def apply(initialItems: String*): Aurkitu =
    Aurkitu(initialItems)
}

object AurkituApp extends App {

  val data = List("reprode", "production", "sares", "deprot")
  val aurkitu = Aurkitu(data)
  println(aurkitu searchChunk "prod")
}
