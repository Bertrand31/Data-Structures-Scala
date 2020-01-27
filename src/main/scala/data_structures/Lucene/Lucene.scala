package data_structures

final case class Lucene(
  private val documents: Map[Int, (String, IndexedSeq[String])] = Map(),
  private val invertedIndex: Map[String, Array[(Int, Int)]] = Map(),
) {

  private val UselessChars = Seq(',', '.', ';', '?', '!', '"')

  private def lineToWords(line: String): Array[String] =
    line.split(" ")
      .map(_.filterNot(UselessChars.contains))
      .map(_.toLowerCase)

  def loadFile(filename: String): Lucene = {
    val document = DocumentLoader.loadDocument(filename)
    val documentId = documents.size
    val documentTuple = (filename, document.map(_._2))
    val newDocuments = documents + (documentId -> documentTuple)
    val newIndex = document.foldLeft(invertedIndex)((acc, tuple) => {
      val (lineNumber, line) = tuple
      lineToWords(line).foldLeft(acc)((index, word) => {
        val set = index.getOrElse(word, Array())
        val documentAndLinePair = (documentId, lineNumber)
        index + (word -> (set :+ documentAndLinePair))
      })
    })
    this.copy(newDocuments, newIndex)
  }

  def loadFiles: IterableOnce[String] => Lucene = _.iterator.foldLeft(this)(_ loadFile _)

  def search(word: String): Array[(Int, Int)] =
    invertedIndex.getOrElse(word.toLowerCase, Array())

  def searchAndShow(word: String): Unit = {
    search(word)
      .groupBy(_._1)
      .foreach(matchTpl => {
        val (documentId, linesMatches) = matchTpl
        val (documentName, lines) = documents(documentId)
        println("")
        println(s"$documentName:")
        linesMatches.map(_._2).distinct.foreach(line => {
          println(s"$line: ${lines(line - 1)}")
        })
      })
  }
}

object LuceneTest extends App {

  val lucene = Lucene() loadFiles Seq("damysos.md", "loremipsum.txt")
  lucene searchAndShow "foo"
}
