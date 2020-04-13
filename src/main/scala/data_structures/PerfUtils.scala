package data_structures

import java.text.NumberFormat

object PerfUtils {

  def time[A](id: String)(block: => A): A = {
    val startTime = System.nanoTime
    val result = block
    val elapsedMicro = Math.round((System.nanoTime() - startTime) / 1000)
    val elapsedStr = if (elapsedMicro > 5000) s"${elapsedMicro / 1000}ms" else s"${elapsedMicro}us"
    println(s"$id took $elapsedStr")
    result
  }

  def profile(id: String)(block: => Any): Long = {

    def printResult(s: String, ns: Long): Unit = {
      val format = NumberFormat.getIntegerInstance.format(_: Long)
      println(s.padTo(16, " ").mkString + format(ns) + " ns")
    }

    var tmpTime = 0L
    val runtimes = (0 to 50000).map(_ => {
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
