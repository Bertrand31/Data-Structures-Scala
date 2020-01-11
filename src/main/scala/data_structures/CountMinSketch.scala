package data_structures

import scala.util.hashing.MurmurHash3.stringHash

final case class CountMinSketch[A](
  private val sketch: Array[Array[Int]],
  private val hashSeed: Int,
  val isEmpty: Boolean = true,
) {

  private lazy val hashFunctions: LazyList[String => Int] =
    (1 until sketch.length)
      .to(LazyList)
      .map(i => (str: String) => Math.abs(stringHash(str, hashSeed + i)) % sketch.head.length)

  def +(item: A): CountMinSketch[A] = {
    val str = item.toString
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

  def ++(items: IterableOnce[A]): CountMinSketch[A] = items.iterator.foldLeft(this)(_ + _)

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

  def apply[A](w: Int, d: Int): CountMinSketch[A] =
    CountMinSketch[A](
      sketch=Array.fill(d)(Array.fill(w)(0)),
      hashSeed=scala.util.Random.nextInt,
    )
}

object CountMinSketchTest {

  import scala.util.Random

  def main(args: Array[String]): Unit = {
    val sketch = CountMinSketch[String](10, 10)
    assert(sketch.isEmpty)
    val un = sketch + "foo"
    assert(!un.isEmpty)
    val deux = un + "bar" + "foo"
    println(deux occurences "bar")
    println(deux occurences "foo")

    val numbersSketch = CountMinSketch[Int](100, 100)
    val stream =
      (1 to 100)
        .to(LazyList)
        .map(_ => Random.between(1, 10))
    val populated = numbersSketch ++ stream
    println(populated occurences 3)
  }
}
