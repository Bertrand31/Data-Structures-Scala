package data_structures

import scala.util.hashing.MurmurHash3.stringHash

final case class CountMinSketch(
  private val sketch: Array[Array[Int]],
  private val hashSeed: Int,
  val isEmpty: Boolean = true,
) {

  private lazy val hashFunctions: LazyList[String => Int] =
    (1 to sketch.length)
      .to(LazyList)
      .map(i => (str: String) => Math.abs(stringHash(str, hashSeed + i)) % sketch.head.length)

  def +(str: String): CountMinSketch = {
    val hashes = hashFunctions.map(_(str))
    val newSketch =
      this.sketch
        .zip(hashes)
        .map(kv => {
          val (row, hash) = kv
          row.updated(hash, row(hash) + 1)
        })
    println(newSketch.toList.map(_.toList))
    this.copy(
      sketch=newSketch,
      isEmpty=false,
    )
  }
}

object CountMinSketch {

  def apply(w: Int, d: Int): CountMinSketch = {
    val sketch = Array.fill(d)(Array.fill(w)(0))
    val hashSeed = scala.util.Random.nextInt
    CountMinSketch(sketch, hashSeed)
  }
}

object CountMinSketchTest {

  def main(args: Array[String]): Unit = {
    val sketch = CountMinSketch(10, 10)
    assert(sketch.isEmpty)
    val un = sketch + "foo"
    val deux = un + "bar" + "foo"
    ()
  }
}
