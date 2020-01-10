package data_structures

import scala.util.hashing.MurmurHash3.stringHash

final case class CountMinSketch[A](
  private val sketch: Array[Array[Int]],
  private val hashSeed: Int,
  val isEmpty: Boolean = true,
) {

  private lazy val hashFunctions: LazyList[String => Int] =
    (1 to sketch.length)
      .to(LazyList)
      .map(i => (str: String) => Math.abs(stringHash(str, hashSeed + i)) % sketch.head.length)

  def +(str: String): CountMinSketch[A] = {
    val hashes = hashFunctions.map(_(str))
    val newSketch =
      this.sketch
        .zip(hashes)
        .map(kv => {
          val (row, hash) = kv
          row.updated(hash, row(hash) + 1)
        })
    this.copy(
      sketch=newSketch,
      isEmpty=false,
    )
  }

  def occurences(item: A): Int = {
    val str = item.toString
    val hashes = hashFunctions.map(_(str))
    this.sketch
      .zip(hashes)
      .foldLeft(Integer.MAX_VALUE)((acc, kv) => {
        val (row, hash) = kv
        acc min row(hash)
      })
  }
}

object CountMinSketch {

  def apply[A](w: Int, d: Int): CountMinSketch[A] = {
    val sketch = Array.fill(d)(Array.fill(w)(0))
    val hashSeed = scala.util.Random.nextInt
    CountMinSketch[A](sketch, hashSeed)
  }
}

object CountMinSketchTest {

  def main(args: Array[String]): Unit = {
    val sketch = CountMinSketch[String](10, 10)
    assert(sketch.isEmpty)
    val un = sketch + "foo"
    val deux = un + "bar" + "foo"
    println(deux occurences "bar")
    println(deux occurences "foo")
  }
}
