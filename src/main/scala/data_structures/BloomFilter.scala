package data_structures

import scala.util.hashing.MurmurHash3.stringHash
import math.{abs, ceil, log, pow, round}
import cats.implicits._

case class BloomFilter[A](
  expectedNumberOfItems: Int,
  falsePositiveProbability: Float,
  private val bitset: BitSet,
  private val maxSize: Int,
  private val numberOfHashFunctions: Int,
  private val hashSeed: Int,
) {

  private def makeHashFn(salt: Int): String => Int =
    (stringHash(_: String, hashSeed |+| salt)) >>> abs >>> (_ % maxSize)

  private def hashFunctions: Iterator[String => Int] =
    (1 to numberOfHashFunctions)
      .iterator
      .map(makeHashFn)

  def +(item: A): BloomFilter[A] = {
    val itemString = item.toString
    copy(bitset=(bitset ++ hashFunctions.map(_(itemString))))
  }

  def `++`: IterableOnce[A] => BloomFilter[A] = _.iterator.foldLeft(this)(_ + _)

  def mayContain(item: A): Boolean = {
    val itemString = item.toString
    hashFunctions.forall(bitset contains _(itemString))
  }

  def approxNumberOfItems: Int = {
    val totalBits = maxSize.toDouble
    round(
      (-totalBits / numberOfHashFunctions.toDouble) * log((1D - (bitset.cardinality.toDouble / totalBits)))
    ).toInt
  }

  def isEmpty: Boolean = bitset.isEmpty
}

object BloomFilter {

  private def getMaxSize(expectedNumberOfItems: Int, falsePositiveProbability: Float): Int =
    ceil {
      val numerator = abs(expectedNumberOfItems * log(falsePositiveProbability)).toDouble
      numerator / log(1D / pow(log(2), 2).toDouble)
    }.toInt

  private def getNumberOfHashFunctions(expectedNumberOfItems: Int, maxSize: Int): Int =
    round(
      (maxSize.toDouble / expectedNumberOfItems.toDouble) * log(2)
    ).toInt

  def apply[A](expectedNumberOfItems: Int, falsePositiveProbability: Float): BloomFilter[A] = {
    val maxSize = getMaxSize(expectedNumberOfItems, falsePositiveProbability)
    BloomFilter(
      expectedNumberOfItems=expectedNumberOfItems,
      falsePositiveProbability=falsePositiveProbability,
      bitset=BitSet(),
      maxSize=maxSize,
      numberOfHashFunctions=getNumberOfHashFunctions(expectedNumberOfItems, maxSize),
      hashSeed=scala.util.Random.nextInt(),
    )
  }
}
