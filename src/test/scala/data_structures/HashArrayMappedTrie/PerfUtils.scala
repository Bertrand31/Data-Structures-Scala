import java.text.NumberFormat
import scala.util.Random
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import data_structures.hamt.HashArrayMappedTrie

object PerfUtils {

  def profile(id: String)(block: => Any): Long = {

    def printResult(s: String, ns: Long): Unit = {
      val format = NumberFormat.getIntegerInstance.format(_: Long)
      println(s.padTo(16, " ").mkString + format(ns) + " ns")
    }

    var tmpTime = 0L
    val runtimes = (0 to 10000000).map(_ => {
      tmpTime = System.nanoTime
      block
      System.nanoTime - tmpTime
    })

    println("============================")
    println(s"Profiling $id:")


    printResult("Cold run", runtimes.head)
    val sortedHotRuns = runtimes.takeRight(runtimes.length / 4).sorted
    printResult("Max hot", sortedHotRuns.last)
    printResult("Min hot", sortedHotRuns.head)
    val median = sortedHotRuns(Math.floor(sortedHotRuns.length / 2).toInt)
    printResult("Med hot", median)
    median
  }
}

class HashArrayMappedTriePerfSpec extends AnyFlatSpec {

  "HAMT get" should "be fast af" in {
    val dataset = (0 to 1000).map(_ => (Random.nextInt, Random.alphanumeric.take(3).mkString))

    val boringMap = dataset.toMap

    val coolMap = HashArrayMappedTrie(dataset)

    val key = dataset(500)._1

    val customHamtTime = PerfUtils.profile("Custom HAMT get") {
      coolMap.get(key)
    }

    val builtInMapTime = PerfUtils.profile("Built-in Map get") {
      boringMap(key)
    }

    val timesFaster = customHamtTime / builtInMapTime
    println(s"Built-in is $timesFaster times faster")
    assert(timesFaster < 50)
  }
}
