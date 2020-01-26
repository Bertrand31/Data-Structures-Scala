package data_structures

final case class Lucene(
  private val documents: Map[Int, (String, IndexedSeq[String])] = Map(),
  private val invertedIndex: Map[String, Set[(Int, Int)]] = Map(),
) {

  private val UselessChars = Seq(',', '.', ';', '?', '!', '"')

  private def lineToWords(line: String): Array[String] =
    line.split(" ").map(_.filterNot(UselessChars.contains))

  def loadFile(filename: String): Lucene = {
    val document = DocumentLoader.loadDocument(filename)
    val documentId = documents.size
    val documentTuple = (filename, document.map(_._2))
    val newDocuments = documents + (documentId -> documentTuple)
    val newIndex = document.foldLeft(invertedIndex)((acc, tuple) => {
      val (lineNumber, line) = tuple
      lineToWords(line).foldLeft(acc)((index, word) => {
        val lowerCaseWord = word.toLowerCase
        val set = index.getOrElse(lowerCaseWord, Set())
        val documentAndLinePair = (documentId, lineNumber)
        index + (lowerCaseWord -> (set + documentAndLinePair))
      })
    })
    this.copy(newDocuments, newIndex)
  }

  def loadFiles: IterableOnce[String] => Lucene = _.iterator.foldLeft(this)(_ loadFile _)

  def search(word: String): Set[(Int, Int)] =
    invertedIndex.getOrElse(word.toLowerCase, Set())

  def searchAndShow(word: String): Unit = {
    val matches = search(word)
    matches foreach (docIdAndLine => {
      val (documentId, lineNumber) = docIdAndLine
      val (documentName, lines) = documents(documentId)
      println(s"$documentName - $lineNumber: ${lines(lineNumber - 1)}")
    })
  }
}

object LuceneTest extends App {

  val lucene = Lucene() loadFile "damysos.md" loadFile "loremipsum.txt"
  lucene searchAndShow "lorem"
  lucene searchAndShow "trie"
}
