package data_structures

import cats.Monoid
import cats.implicits._

object MinimalBug extends App {

  println {
    List(
      Map(
        (0 -> List(8, 9)),
      ),
      Map(
        (0 -> List(10)),
      ),
      Map(
        (1 -> List(30)),
      ),
    ).reduce(_ |+| _)
  }
}
