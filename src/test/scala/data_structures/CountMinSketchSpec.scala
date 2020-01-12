import data_structures.CountMinSketch

import org.scalatest.flatspec.AnyFlatSpec

class CountMinSketchSpec extends AnyFlatSpec {

  behavior of "The CountMinSketch implementation"

  behavior of "the apply method"

  val sketch = CountMinSketch[String](10, 10)

  it should "create an empty CountMinSketch" in {
    assert(sketch.isEmpty)
  }

  val withOne = sketch + "foo"

  behavior of "the + (add) method"

  it should "return a non-empty CountMinSketch" in {
    assert(!withOne.isEmpty)
  }

  behavior of "the occurences method"

  it should "return a number equal or superior to the actual number of occurences" in {
    import scala.util.Random

    val numbersSketch = CountMinSketch[Int](100, 100)
    val indexedSeq = (1 to 10000).toList.map(_ => Random.between(1, 10))
    val populated = numbersSketch ++ indexedSeq
    (1 to 10000).foreach(nb => {
      val count = indexedSeq.count(_ == nb)
      assert(populated.occurences(nb) >= count)
    })
  }

}
