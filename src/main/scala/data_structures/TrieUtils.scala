package data_structures

object TrieUtils {

  def getIndexesFromString: String => Vector[Int] =
    _
      .toLowerCase
      .map(_.toInt)
      .toVector

  def getStringFromIndexes: Vector[Int] => String =
    _
      .map(_.toChar)
      .mkString("")
}
