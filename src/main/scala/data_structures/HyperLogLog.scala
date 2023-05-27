// /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ //
// THIS IS NOT WORKING PROPERLY YET, AND IS A WORK-IN-PROGRESS //
// /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ /!\ //

package data_structures

import scala.util.hashing.MurmurHash3.stringHash
import cats.implicits._

// M: number of counters
case class HyperLogLog(m: Int, M: Int, counters: Array[Int]) {

  def numberOfLeadingZeros(number: Int): Int =
    ((31 - m) to 0 by -1).segmentLength(shift => ((1 << shift) | number) =!= number) + 1

  private def harmonicMean(values: Array[Int]): Double =
    values.size.toFloat / values.map(1F / _.toFloat).sum

  // private def indicator: Array[Int] => Double =
    // 1 / _.map(nb => Math.pow(2, -nb)).sum

  private val correctionConstant: Double =
    M match {
      case 16 => 0.673
      case 32 => 0.697
      case 64 => 0.709
      case _  => 0.7213D / (1D + (1.079D / M.toDouble))
    }

  def +[A](item: A): HyperLogLog = {
    val x = stringHash(item.toString)
    val bits = x.toBinaryString.reverse.padTo(31, '0').reverse
    val (address, rest) = bits.splitAt(math.log(m).toInt)
    val counterAddress = Integer.parseInt(address, 2)
    val leadingZeros = numberOfLeadingZeros(Integer.parseInt(rest, 2))
    val newValue = this.counters(counterAddress) max leadingZeros
    this.copy(counters=this.counters.updated(counterAddress, newValue))
  }

  def ++[A]: IterableOnce[A] => HyperLogLog = _.iterator.foldLeft(this)(_ + _)

  def cardinality: Double = {
    val mean = harmonicMean(this.counters.filterNot(_ === 0))
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

  val data = (1 to 100000).map(_ => scala.util.Random.between(1, 1000))
  val hll = HyperLogLog(4) ++ data
  println("Expected: " ++ data.distinct.size.toString)
  println("Output:   " ++ hll.cardinality.toString)
}
