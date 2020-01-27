package data_structures

object LineSanitizing {

  private val UselessChars = Seq(',', '*', '.', ';', '?', '!', '"', '+', '`', ':', '_')

  def lineToWords: String => Array[String] =
    _
      .split("\\|| |/|\\[|\\]|\\{|\\}|\\\\|\\(|\\)|=| ")
      .map(_.trim)
      .map(_.filterNot(UselessChars.contains))
      .filterNot(_.isEmpty)
      .map(_.toLowerCase)
}
