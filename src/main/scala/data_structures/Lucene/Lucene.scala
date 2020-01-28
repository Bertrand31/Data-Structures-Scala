package data_structures

import cats.implicits._

final case class Lucene(
  private val documents: Map[Int, (String, IndexedSeq[String])] = Map(),
  private val invertedIndex: Map[String, Map[Int, Vector[Int]]] = Map(),
  private val indexesTrie: Trie = Trie(),
) {

  def ingestFile(filename: String): Lucene = {
    val document = DocumentLoader.loadDocument(filename)
    val documentId = documents.size // Using the size of the documents map as an incremental counter
    val newDocuments = documents + (documentId -> ((filename, document.map(_._2))))
    val withNewDocument = this.copy(documents=newDocuments)
    document.foldLeft(withNewDocument)((lucene, line) => {
      val (lineNumber, lineString) = line
      LineSanitizing.lineToWords(lineString).foldLeft(lucene)((acc, word) => {
        val wordOccurences = acc.invertedIndex.getOrElse(word, Map())
        val currentMatches = wordOccurences.getOrElse(documentId, Vector()) :+ lineNumber
        val documentAndLinePair = wordOccurences + (documentId -> currentMatches)
        val newIndex = acc.invertedIndex + (word -> documentAndLinePair)
        val newTrie = acc.indexesTrie + word
        acc.copy(invertedIndex=newIndex, indexesTrie=newTrie)
      })
    })
  }

  def ingestFiles: IterableOnce[String] => Lucene = _.iterator.foldLeft(this)(_ ingestFile _)

  def searchWord(word: String): Map[Int, Vector[Int]] =
    invertedIndex
      .getOrElse(word.toLowerCase, Map())

  def searchPrefix(prefix: String): Map[Int, Vector[Int]] =
    indexesTrie
      .keysWithPrefix(prefix)
      .map(searchWord)
      .foldMap(identity)

  import Console._

  private def printResults: Map[Int, Vector[Int]] => Unit =
    _.foreach(matchTpl => {
      val (documentId, linesMatches) = matchTpl
      val (documentName, lines) = documents(documentId)
      println("")
      println(s"${GREEN}${BOLD}$documentName:${RESET}")
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
