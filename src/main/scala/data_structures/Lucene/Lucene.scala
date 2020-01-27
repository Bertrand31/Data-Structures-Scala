package data_structures

import cats.implicits._

final case class Lucene(
  private val documents: Map[Int, (String, IndexedSeq[String])] = Map(),
  private val invertedIndex: Map[String, Map[Int, Vector[Int]]] = Map(),
  private val indexesTrie: Trie = Trie(),
) {

  private def addToIndex(documentId: Int, document: IndexedSeq[(Int, String)]) =
    document.foldLeft(invertedIndex)((acc, line) => {
      val (lineNumber, lineText) = line
      LineSanitizing.lineToWords(lineText).foldLeft(acc)((index, word) => {
        val wordOccurences = index.getOrElse(word, Map())
        val currentMatches = wordOccurences.getOrElse(documentId, Vector()) :+ lineNumber
        val documentAndLinePair = wordOccurences + (documentId -> currentMatches)
        index + (word -> documentAndLinePair)
      })
    })

  private def addToTrie(document: IndexedSeq[(Int, String)]): Trie =
    document.foldLeft(indexesTrie)((trie, line) => {
      trie ++ LineSanitizing.lineToWords(line._2)
    })

  def ingestFile(filename: String): Lucene = {
    val document = DocumentLoader.loadDocument(filename).filterNot(_._2.isEmpty)
    val documentId = documents.size // Using the size of the documents map as an incremental counter
    this.copy(
      documents=documents + (documentId -> ((filename, document.map(_._2)))),
      invertedIndex=addToIndex(documentId, document),
      indexesTrie=addToTrie(document),
    )
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
  // lucene searchAndShow "foo"
  lucene searchPrefixAndShow "sim"
}
