// /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ //
// THIS IS NOT WORKING PROPERLY YET, AND IS A WORK-IN-PROGRESS //
// /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ //

package data_structures

import scala.util.hashing.MurmurHash3.stringHash
import scala.math.Ordering.Double.TotalOrdering
import cats.implicits._

object HyperLogLog {

  def numberOfLeadingZeros(number: Int): Int =
    (31 to 0 by -1)
      .takeWhile(shift => ((1 << shift) | number) =!= number)
      .size

  private def getMaxPrefixLength[A]: List[A] => Double =
    _.foldLeft(0)((acc, item) =>
      acc max numberOfLeadingZeros(Math.abs(stringHash(item.toString)))
    )

  private def powOfTwo: Double => Double = Math.pow(2, _)

  private def getRegisterCardinality: List[_] => Double = getMaxPrefixLength >>> powOfTwo

  private def harmonicMean(values: Iterable[Double]): Double = values.size / values.map(1 / _).sum

  def getCardinality[A](bits: Int, list: List[A]): Double = {
    val numberOfRegisters = powOfTwo(bits)
    val registersSize = Math.ceil(list.length / numberOfRegisters).toInt
    val registers = list.sliding(registersSize, registersSize)
    harmonicMean(
      registers
        .map(getRegisterCardinality)
        .toArray
        .sorted
        .take(Math.ceil(numberOfRegisters * 0.7).toInt)
    )
  }
}

 object HyperLogLogApp extends App {

  val randNumbers = (1 to 100000).map(_ => scala.util.Random.between(1, 1000))
  val estimatedCardinality = HyperLogLog.getCardinality(4, randNumbers.toList)
  println(estimatedCardinality)
 }
