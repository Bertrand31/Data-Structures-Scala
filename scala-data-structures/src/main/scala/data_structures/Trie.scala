package data_structures

final case class Trie(
  private val children: Array[Option[Trie]],
  private val isFinal: Boolean
) {

  private def getIndexes: String => Seq[Int] =
    _
      .toCharArray
      .map(_.toLower.charValue - 97) // 'a' is 97, 'b' is 98, etc
      .toList

  private def getStringFromIndexes: Seq[Int] => String =
    _
      .map(_ + 97)
      .map(_.toChar)
      .mkString("")

  def +=(word: String): Trie = {
    def insertIndexes(indexes: Seq[Int], trie: Trie): Trie = {
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
    }

    insertIndexes(getIndexes(word), this)
  }

  def ++=(words: Seq[String]): Trie = words.foldLeft(this)((acc, word) => acc += word)

  def ++(trie: Trie): Trie = this ++= trie.keys

  def contains(word: String): Boolean = {
    def endsOnLastIndex(indexes: Seq[Int], trie: Trie): Boolean =
      indexes match {
        case head +: Nil => trie.children(head).map(_.isFinal).getOrElse(false)
        case head +: tail => trie.children(head).map(endsOnLastIndex(tail, _)).getOrElse(false)
      }

    endsOnLastIndex(getIndexes(word), this)
  }

  def keys(): List[String] = {

    def descendCharByChar(accumulator: Vector[Int], trie: Trie): List[Vector[Int]] = {
      (0 to 25).flatMap(index => {
        trie.children(index) match {
          case None => Vector()
          case Some(subTrie) if (subTrie.isFinal) => {
            val currentWord = accumulator :+ index
            currentWord +: descendCharByChar(currentWord, subTrie)
          }
          case Some(subTrie) => descendCharByChar(accumulator :+ index, subTrie)
        }
      }).toList
    }

    descendCharByChar(Vector(), this).map(getStringFromIndexes)
  }

  def keysWithPrefix(prefix: String): List[String] = {

    def descendWithPrefix(indexes: Seq[Int], trie: Trie): Option[Trie] =
      indexes match {
        case head +: Nil => trie.children(head)
        case head +: tail => trie.children(head).flatMap(descendWithPrefix(tail, _))
      }

    val subTrie = descendWithPrefix(getIndexes(prefix), this)
    subTrie match {
      case None => List()
      case Some(subTrie) if (subTrie.isFinal) => prefix +: subTrie.keys.map(prefix + _)
      case Some(subTrie) => subTrie.keys.map(prefix + _)
    }
  }

  def isEmpty(): Boolean = !this.children.exists(_.isDefined)
}

object Trie {

  private val LatinAlphabetLength = 26

  def apply(initialItems: String*): Trie =
    Trie(
      children=Array.fill[Option[Trie]](LatinAlphabetLength)(None),
      isFinal=false
    ) ++= initialItems
}

object TrieTest {

  def main(args: Array[String]): Unit = {
    val newTrie = Trie()
    assert(newTrie.isEmpty == true)
    val trieWithBar = newTrie += "bar"
    assert(newTrie.isEmpty == true) // No mutation
    assert(trieWithBar.isEmpty == false)
    assert(trieWithBar.contains("bar") == true)
    assert(trieWithBar.contains("baz") == false)
    val trieWithFooBar = trieWithBar += "foo"
    assert(trieWithFooBar.keys == List("bar", "foo"))
    val complexTrie = trieWithFooBar += "barreau"
    assert(complexTrie.keys == List("bar", "barreau", "foo"))
    assert(complexTrie.keysWithPrefix("barr") == List("barreau"))
    assert(complexTrie.keysWithPrefix("bar") == List("bar", "barreau"))
    assert(complexTrie.keysWithPrefix("baz") == List())
    val sampleTrie = Trie("foo", "bar")
    assert(sampleTrie.keys == List("bar", "foo"))
    val sampleTrie2 = Trie("baz")
    val mergedTrie = sampleTrie ++ sampleTrie2
    assert(mergedTrie.keys == List("bar", "baz", "foo"))
  }
}