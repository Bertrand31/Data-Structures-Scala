package data_structures

final case class IntervalTree(
  private val centerPoint: Int,
  private val left: Option[IntervalTree],
  private val right: Option[IntervalTree],
  private val overlappingLeftSorted: List[(Int, Int)],
  private val overlappingRightSorted: List[(Int, Int)]
) {

  import IntervalTree.{Interval, Point}

  def getOverlappingFromPoint(point: Point, tree: IntervalTree = this): List[Interval] = {
    if (point < centerPoint) { // Go left
      val centerOverlaps = tree.overlappingLeftSorted.takeWhile(t => t._1 <= point && t._2 >= point)
      tree.left.fold(centerOverlaps)(centerOverlaps ++ getOverlappingFromPoint(point, _))
    } else if (point > centerPoint) { // Go right
      val centerOverlaps = tree.overlappingRightSorted.takeWhile(t => t._2 >= point && t._1 <= point)
      tree.right.fold(centerOverlaps)(centerOverlaps ++ getOverlappingFromPoint(point, _))
    } else { // Take all the intervals overlapping with the centerPoint
      tree.overlappingLeftSorted
    }
  }

  def getOverlappingFromInterval(interval: Interval, tree: IntervalTree = this): List[Interval] = {
    if (interval._2 < tree.centerPoint) {
      val leftOverlaps = tree.left.fold(List[Interval]())(getOverlappingFromInterval(interval, _))
      tree.overlappingRightSorted.takeWhile(t => t._2 >= interval._2 && t._1 <= interval._1) ++ leftOverlaps
    } else if (interval._1 > tree.centerPoint) {
      val rightOverlaps = tree.right.fold(List[Interval]())(getOverlappingFromInterval(interval, _))
      tree.overlappingLeftSorted.takeWhile(t => t._1 <= interval._1 && t._2 >= interval._2) ++ rightOverlaps
    } else {
      tree.overlappingLeftSorted
    }
  }
}

object IntervalTree {

  type Point = Int
  type Interval = (Point, Point)

  def apply(intervals: List[Interval]): IntervalTree = {
    val min = intervals.minBy(_._1)._1
    val max = intervals.maxBy(_._2)._2
    val centerPoint = min + Math.ceil((max - min) / 2).toInt
    val strictlyLeft = intervals.filter(_._2 < centerPoint)
    val strictlyRight = intervals.filter(_._1 > centerPoint)
    val left = if (strictlyLeft.isEmpty) None else Some(this.apply(strictlyLeft))
    val right = if (strictlyRight.isEmpty) None else Some(this.apply(strictlyRight))
    val middle = intervals.filter(tuple => tuple._1 <= centerPoint && tuple._2 >= centerPoint)
    val overlappingLeftSorted = middle.sortBy(_._1)
    val overlappingRightSorted = middle.sortBy(_._2)
    IntervalTree(centerPoint, left, right, overlappingLeftSorted, overlappingRightSorted)
  }
}

object IntervalTreeTest {

  def main(args: Array[String]): Unit = {
    val tree = IntervalTree(List((1,8), (7, 20), (4, 12)))
    assert(tree.getOverlappingFromPoint(2) == List((1, 8)))
    assert(tree.getOverlappingFromPoint(8) == List((4, 12), (7, 20), (1, 8)))
    assert(tree.getOverlappingFromPoint(6) == List((4, 12), (1, 8)))
    assert(tree.getOverlappingFromInterval((1, 2)) == List((1, 8)))
    assert(tree.getOverlappingFromInterval((9, 9)) == List((4, 12), (7, 20)))
    assert(tree.getOverlappingFromInterval((1, 1)) == List((1, 8)))
    assert(tree.getOverlappingFromInterval((1, 2)) == List((1, 8)))
    println(tree.getOverlappingFromInterval((1, 200)))
    assert(tree.getOverlappingFromInterval((1, 200)) == List((4, 12), (7, 20), (1, 8)))
    val tree2 = IntervalTree(List((1, 2), (7, 10), (19, 20)))
    assert(tree2.getOverlappingFromPoint(8) == List((7, 10)))
  }
}
