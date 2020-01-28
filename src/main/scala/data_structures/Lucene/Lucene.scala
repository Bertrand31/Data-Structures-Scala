package data_structures

import scala.collection.immutable.ArraySeq
import cats.implicits._

final case class Lucene(
  private val documents: Map[Int, String] = Map(),
  private val invertedIndex: Map[String, Map[Int, ArraySeq[Int]]] = Map(),
  private val indexesTrie: Trie = Trie(),
) {

  import ArraySeqMonoid._

  private def ingestLine(docId: Int)(lucene: Lucene, line: (Int, String)): Lucene = {
    val (lineNumber, lineString) = line
    val words = LineSanitizing.lineToWords(lineString)
    val newTrie = lucene.indexesTrie ++ words
    val newIndex = words.foldLeft(lucene.invertedIndex)((index, word) => {
      val wordOccurences = index.getOrElse(word, Map())
      val currentMatches = wordOccurences.getOrElse(docId, ArraySeq()) :+ lineNumber
      val documentAndLinePair = wordOccurences + (docId -> currentMatches)
      index + (word -> documentAndLinePair)
    })
    lucene.copy(invertedIndex=newIndex, indexesTrie=newTrie)
  }

  def ingestFile(filename: String): Lucene = {
    val document = DocumentLoader.loadDocumentWithLinesNumbers(filename)
    val documentId = this.documents.size // Using the size of the documents map as a counter ID
    document
      .foldLeft(this)(ingestLine(documentId))
      .copy(documents=this.documents + (documentId -> filename))
  }

  def ingestFiles: IterableOnce[String] => Lucene = _.iterator.foldLeft(this)(_ ingestFile _)

  def searchWord(word: String): Map[Int, ArraySeq[Int]] =
    invertedIndex.getOrElse(word.toLowerCase, Map())

  def searchPrefix(prefix: String): Map[Int, ArraySeq[Int]] =
    indexesTrie
      .keysWithPrefix(prefix)
      .map(searchWord)
      .foldMap(identity)

  import Console._

  private def printResults: Map[Int, ArraySeq[Int]] => Unit =
    _.foreach(matchTpl => {
      val (documentId, linesMatches) = matchTpl
      val filename = documents(documentId)
      val lines = DocumentLoader.loadDocument(filename).take(linesMatches.max).toArray
      println(s"\n${GREEN}${BOLD}$filename:${RESET}")
      linesMatches.distinct.foreach(line => {
        println(s"${YELLOW}$line${RESET}: ${lines(line - 1)}")
      })
    })

  def searchAndShow: String => Unit = printResults compose searchWord

  def searchPrefixAndShow: String => Unit = printResults compose searchPrefix
}

object LuceneTest extends App {

  val lucene = Lucene() ingestFiles Seq("damysos.md", "loremipsum.txt")
  lucene searchAndShow "foo"
  lucene searchPrefixAndShow "sim"
}
