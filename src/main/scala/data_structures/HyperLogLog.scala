// /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ //
// THIS IS NOT WORKING PROPERLY YET, AND IS A WORK-IN-PROGRESS //
// /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ //

package data_structures

import scala.util.hashing.MurmurHash3.stringHash
import cats.implicits._

case class HyperLogLog(m: Int, M: Int, counters: Array[Int]) {

  def numberOfLeadingZeros(number: Int): Int =
    ((31 - m) to 0 by -1).segmentLength(shift => ((1 << shift) | number) =!= number) + 1

  // private def harmonicMean(values: Array[Int]): Double =
    // values.size.toFloat / values.map(1F / _.toFloat).sum

  private def indicator: Array[Int] => Double =
    1 / _.map(nb => Math.pow(2, -nb)).sum

  private val correctionConstant: Double =
    M match {
      case 16 => 0.673
      case 32 => 0.697
      case 64 => 0.709
      case M  => 0.7213D / (1D + (1.079D / M.toDouble))
    }

  def +[A](item: A): HyperLogLog = {
    val x = stringHash(item.toString)
    val bits = x.toBinaryString.reverse.padTo(31, '0').reverse
    val counterAddress = Integer.parseInt(bits.take(m), 2)
    val leadingZeros = numberOfLeadingZeros(Integer.parseInt(bits.drop(m), 2))
    val newValue = this.counters(counterAddress) max leadingZeros
    this.copy(counters=this.counters.updated(counterAddress, newValue))
  }

  def ++[A]: IterableOnce[A] => HyperLogLog = _.iterator.foldLeft(this)(_ + _)

  def cardinality: Double = {
    val mean = indicator(this.counters.filterNot(_ === 0))
    mean * correctionConstant * Math.pow(m, 2)
  }
}

object HyperLogLog {

  def apply(m: Int): HyperLogLog = {
    val M = Math.pow(2, m).toInt // Number of counters
    val counters = new Array[Int](M)
    HyperLogLog(m, M, counters)
  }
}

 object HyperLogLogApp extends App {

  val hll = HyperLogLog(4) ++ (1 to 100000).map(_ => scala.util.Random.between(1, 100))
  println(hll.cardinality)
 }
