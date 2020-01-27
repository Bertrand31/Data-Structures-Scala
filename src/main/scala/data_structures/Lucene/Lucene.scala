package data_structures

final case class Lucene(
  private val documents: Map[Int, (String, IndexedSeq[String])] = Map(),
  private val invertedIndex: Map[String, Map[Int, Array[Int]]] = Map(),
) {

  private val UselessChars = Seq(',', '.', ';', '?', '!', '"')

  private def lineToWords(line: String): Array[String] =
    line.split(" ")
      .map(_.filterNot(UselessChars.contains))
      .map(_.toLowerCase)

  private def ingestDocument(documentId: Int, document: IndexedSeq[(Int, String)]) =
    document.foldLeft(invertedIndex)((acc, line) => {
      val (lineNumber, lineText) = line
      lineToWords(lineText).foldLeft(acc)((index, word) => {
        val wordOccurences = index.getOrElse(word, Map())
        val currentMatches = wordOccurences.getOrElse(documentId, Array()) :+ lineNumber
        val documentAndLinePair = wordOccurences + (documentId -> currentMatches)
        index + (word -> documentAndLinePair)
      })
    })

  def loadFile(filename: String): Lucene = {
    val document = DocumentLoader.loadDocument(filename)
    val documentId = documents.size // Using the size of the documents map as an incremental counter
    this.copy(
      documents=documents + (documentId -> ((filename, document.map(_._2)))),
      invertedIndex=ingestDocument(documentId, document),
    )
  }

  def loadFiles: IterableOnce[String] => Lucene = _.iterator.foldLeft(this)(_ loadFile _)

  def search(word: String): Map[Int, Array[Int]] =
    invertedIndex.getOrElse(word.toLowerCase, Map())

  import Console._

  def searchAndShow(word: String): Unit =
    search(word).foreach(matchTpl => {
      val (documentId, linesMatches) = matchTpl
      val (documentName, lines) = documents(documentId)
      println("")
      println(s"${GREEN}${BOLD}$documentName:${RESET}")
      linesMatches.distinct.foreach(line => {
        println(s"${YELLOW}$line${RESET}: ${lines(line - 1)}")
      })
    })
}

object LuceneTest extends App {

  val lucene = Lucene() loadFiles Seq("damysos.md", "loremipsum.txt")
  lucene searchAndShow "foo"
}
