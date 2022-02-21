package data_structures

import scala.util.Random
import data_structures.hamt.HashArrayMappedTrie
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

object States {

  @State(Scope.Thread)
  class MyState {

    val dataset = (0 to 1000).map(_ => (Random.nextInt(), Random.alphanumeric.take(3).mkString))
    val key = dataset(500)._1

    val boringMap = dataset.toMap
    val coolMap = HashArrayMappedTrie(dataset)
  }
}

@BenchmarkMode(Array(Mode.Throughput))
@Measurement(iterations = 4, timeUnit = TimeUnit.SECONDS, time = 1)
@Warmup(iterations = 4, timeUnit = TimeUnit.SECONDS, time = 1)
@Fork(value = 2, jvmArgsAppend = Array())
@Threads(value = 1)
@OutputTimeUnit(TimeUnit.SECONDS)
class HAMTConstruction {

  @Benchmark
  def customHAMTConstruction(state: States.MyState, blackhole: Blackhole): Unit = {
    val map = HashArrayMappedTrie(state.dataset)
    blackhole.consume(map)
  }

  @Benchmark
  def builtinHAMTConstruction(state: States.MyState, blackhole: Blackhole): Unit = {
    val map = Map.from(state.dataset)
    blackhole.consume(map)
  }
}

@BenchmarkMode(Array(Mode.Throughput))
@Measurement(iterations = 8, timeUnit = TimeUnit.MILLISECONDS, time = 100)
@Warmup(iterations = 8, timeUnit = TimeUnit.MILLISECONDS, time = 100)
@Fork(value = 2, jvmArgsAppend = Array())
@Threads(value = 1)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class HAMTGet {

  @Benchmark
  def customHAMTGet(state: States.MyState, blackhole: Blackhole): Unit = {
    val newMap = state.coolMap.get(state.key)
    blackhole.consume(newMap)
  }

  @Benchmark
  def builtinHAMTGet(state: States.MyState, blackhole: Blackhole): Unit = {
    val newMap = state.boringMap.get(state.key)
    blackhole.consume(newMap)
  }
}
