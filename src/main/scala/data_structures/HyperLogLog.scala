// /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\
// THIS IS NOT WORKING PROPERLY YET AND IS A WORK-IN-PROGRESS
// /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\

package data_structures

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3.stringHash
import scala.math.Ordering.Double.TotalOrdering

object HyperLogLog {

  @tailrec
  private def getMaxPrefixLength[A](list: List[A], longuestPrefix: Int = 1): Double = {
    val head +: tail = list
    val headPrefixLength =
      stringHash(head.toString)
        .toBinaryString
        .reverse
        .padTo(32, '0')
        .reverse
        .takeWhile(_ == '0')
        .length + 1
    val newLonguestPrefix = Math.max(longuestPrefix, headPrefixLength)
    if (tail.isEmpty) newLonguestPrefix
    else getMaxPrefixLength(tail, newLonguestPrefix)
  }

  private def powOfTwo: Double => Double = Math.pow(2, _)

  private def getRegisterCardinality: List[_] => Double =
    (getMaxPrefixLength(_: List[_])) andThen powOfTwo

  private def harmonicMean(values: Seq[Double]): Double =
    values.length / values.map(1 / _).sum

  def getCardinality[A](bits: Int, list: List[A]): Double = {
    val numberOfRegisters = powOfTwo(bits)
    val registersSize = Math.ceil(list.length / numberOfRegisters).toInt
    harmonicMean {
      list
        .sliding(registersSize, registersSize)
        .map(getRegisterCardinality)
        .toIndexedSeq
        .sorted
        .take(Math.ceil(numberOfRegisters * 0.7).toInt)
    }
  }
}

 object HyperLogLogApp extends App {

  val randNumbers = (1 to 1000).map(_ => scala.util.Random.between(1, 10))
  val estimatedCardinality = HyperLogLog.getCardinality(4, randNumbers.toList)
  println(estimatedCardinality)
 }
