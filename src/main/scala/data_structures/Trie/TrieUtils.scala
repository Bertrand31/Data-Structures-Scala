package data_structures.trie

object TrieUtils {

  def getIndexesFromString: String => Vector[Int] =
    _
      .toLowerCase
      .toCharArray
      .map(_.toInt)
      .toVector

  def getStringFromIndexes: Seq[Int] => String =
    _
      .map(_.toChar)
      .mkString("")
}
