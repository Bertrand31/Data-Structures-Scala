package data_structures

import scala.util.Random
import scala.util.hashing.MurmurHash3.stringHash

final case class CountMinSketch[A](
  private val sketch: Array[Array[Int]],
  private val hashSeed: Int,
  isEmpty: Boolean = true,
) {

  private def hashFunctions: Iterator[String => Int] =
    (1 to sketch.length)
      .iterator
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
