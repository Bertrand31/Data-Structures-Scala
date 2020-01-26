package data_structures

import scala.io.Source

object DocumentLoader {

  private val BaseDirectory = "src/main/scala/data_structures/Lucene/documents"

  def loadDocument(filename: String): IndexedSeq[(Int, String)] = {
    val lines =
      Source
        .fromFile(s"$BaseDirectory/$filename")
        .getLines
        .toArray
    (1 to lines.length) zip lines
  }
}
