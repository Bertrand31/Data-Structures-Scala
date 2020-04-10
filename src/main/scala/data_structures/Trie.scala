package data_structures

final case class Trie(
  private val children: Array[Option[Trie]],
  private val isFinal: Boolean,
) {

  private def getIndexesFromString: String => Seq[Int] =
    _
      .toLowerCase
      .toCharArray
      .map(_.toInt)
      .toList

  private def getStringFromIndexes: Seq[Int] => String =
    _
      .map(_.toChar)
      .mkString("")

  def +(word: String): Trie = {
    def insertIndexes(indexes: Seq[Int], trie: Trie): Trie =
      indexes match {
        case head +: Nil => {
          val newSubTrie = trie.children(head).getOrElse(Trie()).copy(isFinal=true)
          trie.copy(trie.children.updated(head, Some(newSubTrie)))
        }
        case head +: tail => {
          val newSubTrie = trie.children(head) match {
            case Some(subTrie) => insertIndexes(tail, subTrie)
            case None => insertIndexes(tail, Trie())
          }
          trie.copy(trie.children.updated(head, Some(newSubTrie)))
        }
      }

    insertIndexes(getIndexesFromString(word), this)
  }

  def ++(words: IterableOnce[String]): Trie = words.iterator.foldLeft(this)(_ + _)

  def ++(trie: Trie): Trie = this ++ trie.keys

  def contains(word: String): Boolean = {
    def endsOnLastIndex(indexes: Seq[Int], trie: Trie): Boolean =
      indexes match {
        case head +: Nil => trie.children(head).map(_.isFinal).getOrElse(false)
        case head +: tail => trie.children(head).map(endsOnLastIndex(tail, _)).getOrElse(false)
      }

    endsOnLastIndex(getIndexesFromString(word), this)
  }

  def keys(): List[String] = {
    def descendCharByChar(accumulator: Vector[Int], trie: Trie): List[Vector[Int]] =
      (0 to (trie.children.length - 1)).flatMap(index => {
        trie.children(index) match {
          case None => Vector()
          case Some(subTrie) if (subTrie.isFinal) => {
            val currentWord = accumulator :+ index
            currentWord +: descendCharByChar(currentWord, subTrie)
          }
          case Some(subTrie) => descendCharByChar(accumulator :+ index, subTrie)
        }
      }).toList

    descendCharByChar(Vector(), this).map(getStringFromIndexes)
  }

  def firstNKeys(n: Int): List[String] = {
    def takeNWords(accumulator: Vector[Int], trie: Trie, soFar: List[Vector[Int]]): List[Vector[Int]] =
      (0 to (trie.children.size - 1)).foldLeft(soFar)((acc, index) => {
        if (acc.size >= n) acc
        else
          trie.children(index) match {
            case Some(subTrie) if (subTrie.isFinal) =>
              val currentWord = accumulator :+ index
              val newSoFar = acc :+ currentWord
              if (newSoFar.size >= n) newSoFar
              else takeNWords(currentWord, subTrie, newSoFar)
            case Some(subTrie) =>
              takeNWords(accumulator :+ index, subTrie, acc)
            case None => acc
          }
      })

    takeNWords(Vector(), this, List.empty).map(getStringFromIndexes)
  }

  def keysWithPrefix(prefix: String, max: Option[Int]): List[String] = {

    def descendWithPrefix(indexes: Seq[Int], trie: Trie): Option[Trie] =
      indexes match {
        case head +: Nil => trie.children(head)
        case head +: tail => trie.children(head).flatMap(descendWithPrefix(tail, _))
      }

    val subTrie = descendWithPrefix(getIndexesFromString(prefix), this)
    subTrie match {
      case None => List()
      case Some(subTrie) if (subTrie.isFinal) =>
        max match {
          case Some(maxWords) => prefix +: subTrie.firstNKeys(maxWords - 1).map(prefix + _)
          case None => prefix +: subTrie.keys.map(prefix + _)
        }
      case Some(subTrie) =>
        max match {
          case Some(maxWords) => subTrie.firstNKeys(maxWords).map(prefix + _)
          case None => subTrie.keys.map(prefix + _)
        }
    }
  }

  lazy val isEmpty: Boolean = !this.children.exists(_.isDefined)
}

object Trie {

  private val LatinAlphabetLength = 122

  def apply(initialItems: String*): Trie =
    Trie(
      children=Array.fill[Option[Trie]](LatinAlphabetLength)(None),
      isFinal=false,
    ) ++ initialItems
}

object TrieTest extends App {

  val data = List(
    "project runway",
    "pinterest",
    "river",
    "kayak",
    "progenex",
    "progeria",
    "pg&e",
    "project free tv",
    "bank",
    "proactive",
    "progesterone",
    "press democrat",
    "priceline",
    "pandora",
    "reprobe",
    "paypal",
  )
  val trie = Trie(data:_*)
  println(trie.keysWithPrefix("p", Some(4)))
  println(trie.keysWithPrefix("p", None))
}
