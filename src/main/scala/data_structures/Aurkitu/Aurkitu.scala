package data_structures

final case class Aurkitu(
  private val words: Map[Int, String]               = Map.empty,
  private val characterPositions: Map[Char, BitSet] = Map.empty,
  private val index: Map[(Char, Int), BitSet]       = Map.empty,
) {

  def +(word: String): Aurkitu = {
    val wordId = this.words.size
    val newWords = words + (wordId -> word)
    val pairs = word.zipWithIndex
    val newCharacterPositions = pairs.foldLeft(this.characterPositions)((map, pair) => {
      val (char, charPosition) = pair
      val newBitSet = map.getOrElse(char, BitSet()) add charPosition
      map.updated(char, newBitSet)
    })
    val newIndex = pairs.foldLeft(this.index)((acc, pair) => {
      val newBitSet = acc.getOrElse(pair, BitSet()) add wordId
      acc.updated(pair, newBitSet)
    })
    Aurkitu(newWords, newCharacterPositions, newIndex)
  }

  def `++`: IterableOnce[String] => Aurkitu = _.iterator.foldLeft(this)(_ + _)

  def searchChunk: String => Set[String] =
    _
      .toIndexedSeq
      .map(char =>
        this.characterPositions
          .get(char)
          .fold(Set.empty[String]) {
            _
              .iterator
              .map((char, _))
              .flatMap(this.index.get)
              .flatMap(_.iterator)
              .map(this.words)
              .toSet
          }
      )
      .reduce(_ intersect _)

  def toIterable: Iterable[String] = this.words.values
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
