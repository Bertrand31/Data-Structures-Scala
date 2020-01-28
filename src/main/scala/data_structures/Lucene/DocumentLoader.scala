package data_structures

import scala.io.Source

object DocumentLoader {

  private val BaseDirectory = "src/main/scala/data_structures/Lucene/documents"

  def loadDocument(filename: String): Iterator[String] =
    Source
      .fromFile(s"$BaseDirectory/$filename")
      .getLines

  def loadDocumentWithLinesNumbers(filename: String): IndexedSeq[(Int, String)] = {
    val lines = loadDocument(filename).toArray
    (1 to lines.length) zip lines
  }
}
