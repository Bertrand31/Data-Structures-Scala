package data_structures

import scala.util.Random
import scala.util.hashing.MurmurHash3.stringHash

final case class CountMinSketch[A](
  private val sketch: Array[Array[Int]],
  private val hashSeed: Int,
  isEmpty: Boolean = true,
) {

  private def hashFunctions: LazyList[String => Int] =
    (1 to sketch.length)
      .to(LazyList)
      .map(i => (str: String) => Math.abs(stringHash(str, hashSeed + i)) % sketch.head.length)

  def +(item: A): CountMinSketch[A] = {
    val str = item.toString
    val hashes = hashFunctions.map(_(str))
    val newSketch =
      this.sketch
        .zip(hashes)
        .map({ case (row, hash) => row.updated(hash, row(hash) + 1) })
    this.copy(
      sketch=newSketch,
      isEmpty=false,
    )
  }

  def ++(items: IterableOnce[A]): CountMinSketch[A] = items.iterator.foldLeft(this)(_ + _)

  def occurences(item: A): Int = {
    val str = item.toString
    hashFunctions
      .map(_(str))
      .zip(this.sketch)
      .map({ case (hash, row) => row(hash) }) // Still a lazy list at this point
      .min // This forces the evaluation
  }
}

object CountMinSketch {

  def apply[A](width: Int, depth: Int): CountMinSketch[A] =
    CountMinSketch[A](
      sketch=Array.fill(depth)(Array.fill(width)(0)),
      hashSeed=Random.nextInt,
    )
}

object CountMinSketchTest extends App {

  val sketch = CountMinSketch[String](10, 10)
  assert(sketch.isEmpty)
  val un = sketch + "foo"
  assert(!un.isEmpty)
  val deux = un + "bar" + "foo"
  println(deux occurences "bar")
  println(deux occurences "foo")

  val numbersSketch = CountMinSketch[Int](100, 100)
  val indexedSeq = (1 to 10000).map(_ => Random.between(1, 10))
  val populated = numbersSketch ++ indexedSeq
  println(populated occurences 3)
  println(indexedSeq.count(_ == 3))
}
