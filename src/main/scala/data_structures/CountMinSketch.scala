package data_structures

import scala.util.Random
import scala.util.hashing.MurmurHash3.stringHash
import Utils.AugmentedArray

final case class CountMinSketch[A](
  private val width: Int,
  private val depth: Int,
  private val sketch: Array[Array[Int]],
  private val hashSeed: Int,
  isEmpty: Boolean = true,
) {

  private def hashFunctions: Iterator[String => Int] =
    (1 to width)
      .iterator
      .map(i => (str: String) => Math.abs(stringHash(str, hashSeed + i)) % depth)

  def +(item: A): CountMinSketch[A] = {
    val str = item.toString
    val hashes = hashFunctions.map(_(str))
    val newSketch =
      this.sketch
        .zip(hashes)
        .map({ case (row, hash) => row.updatedWith(hash, _ + 1) })
    this.copy(sketch=newSketch, isEmpty=false)
  }

  def ++(items: IterableOnce[A]): CountMinSketch[A] = items.iterator.foldLeft(this)(_ + _)

  def occurences(item: A): Int = {
    val str = item.toString
    val (hash, row) =
      hashFunctions
        .map(_(str))
        .zip(this.sketch)
        .minBy({ case (hash, row) => row(hash) })
    row(hash)
  }
}

object CountMinSketch {

  def apply[A](width: Int, depth: Int): CountMinSketch[A] =
    CountMinSketch[A](
      width=width,
      depth=depth,
      sketch=Array.fill(depth)(Array.fill(width)(0)),
      hashSeed=Random.nextInt(),
    )
}
