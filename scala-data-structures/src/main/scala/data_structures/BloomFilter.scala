package data_structures

import scala.util.hashing.MurmurHash3
import scala.util.Random

case class BloomFilter[A](
  nbOfItems: Int,
  falsePositiveProbability: Float,
  private val array: Array[Boolean],
  private val hashFunctions: List[String => Int]
) {

  def +=(elem: A): BloomFilter[A] = {
    val str = elem.toString
    val hashes = this.hashFunctions.map(_(str))
    val newArr = hashes.foldLeft(this.array)((acc, hash) => acc.updated(hash, true))
    BloomFilter(nbOfItems, falsePositiveProbability, newArr, hashFunctions)
  }

  def ++=(elems: Seq[A]): BloomFilter[A] = elems.foldLeft(this)(_ += _)

  def contains(elem: A): Boolean = {
    val str = elem.toString
    this.hashFunctions.forall(fn => this.array(fn(str)))
  }
}

object BloomFilter {

  import Math.{abs, ceil, log, pow, round}

  private def getArraySize(nbOfItems: Int, falsePositiveProbability: Float): Int =
    ceil(
      abs(nbOfItems * log(falsePositiveProbability)) / log(1 / pow(log(2), 2))
    ).toInt

  private def getHashFunctionsNumber(nbOfItems: Int, arraySize: Int): Int =
    round(
      (arraySize / nbOfItems) * log(2)
    ).toInt

  def apply[A](nbOfItems: Int, falsePositiveProbability: Float): BloomFilter[A] = {
    val arraySize = getArraySize(nbOfItems, falsePositiveProbability)
    val array = Array.ofDim[Boolean](arraySize)

    val nbOfHashFunctions = getHashFunctionsNumber(nbOfItems, arraySize)
    val hashFunctions: List[String => Int] =
      (1 to nbOfHashFunctions).toList.map(_ => {
        val seed = Random.nextInt
        (str: String) => Math.abs(MurmurHash3.stringHash(str, seed)) % arraySize
      })
    BloomFilter(nbOfItems, falsePositiveProbability, array, hashFunctions)
  }
}

object BloomFilterTest {

  def main(args: Array[String]): Unit = {
    val empty = BloomFilter[Int](4000, 0.0000001f)
    val withOne = empty += 4
    val withThree = withOne ++= Vector(5, 2910)
    println(withThree contains 4)
    println(withThree contains 5)
    println(withThree contains 2910)
    println(withThree contains 211)
  }
}
