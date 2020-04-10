package data_structures.trie

object TrieUtils {

  def getIndexesFromString: String => Seq[Int] =
    _
      .toLowerCase
      .toCharArray
      .map(_.toInt)
      .toList

  def getStringFromIndexes: Seq[Int] => String =
    _
      .map(_.toChar)
      .mkString("")
}
