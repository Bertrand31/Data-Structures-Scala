package data_structures

final case class RadixTree(
  private val children: Map[String, RadixTree] = Map.empty,
  private val chunk: String = "",
  private val isWord: Boolean = false,
) {

  def +(str: String): RadixTree =
    (1 to str.size).find(prefixLength => {
      val substr = str.take(prefixLength)
      this.children.contains(substr)
    }) match {
      case Some(prefixLength) =>
        val (prefix, rest) = str.splitAt(prefixLength)
        this.children(prefix) + rest
      case None =>
        val newChildren = this.children + (str -> new RadixTree(isWord=true))
        this.copy(children=newChildren)
    }

  def `++`: IterableOnce[String] => RadixTree = _.iterator.foldLeft(this)(_ + _)
}

object RadixTree {

  def apply: IterableOnce[String] => RadixTree = new RadixTree ++ _

  def apply(initialItems: String*): RadixTree = apply(initialItems)
}

object RadixTreeTest extends App {

  val tree = RadixTree() ++ List("trumped", "trumpet", "trumpistan")
  println(tree)
}
