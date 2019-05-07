package data_structures

final case class IntervalTree(
  private val centerPoint: Int,
  private val left: Option[IntervalTree],
  private val right: Option[IntervalTree],
  private val overlappingLeftSorted: List[(Int, Int)],
  private val overlappingRightSorted: List[(Int, Int)]
) {

  import IntervalTree.{Interval, Point}

  def getOverlappingFromPoint(point: Point): List[Interval] = {
    def getIntervals(point: Point, tree: Option[IntervalTree]): List[Interval] =
      tree match {
        case None => List()
        case Some(tree) => {
          val direction = point - tree.centerPoint
          if (point < centerPoint) { // Go left
            tree.overlappingLeftSorted.takeWhile(_._1 <= point) ++ getIntervals(point, tree.left)
          } else if (point > centerPoint) { // Go right
            tree.overlappingRightSorted.takeWhile(_._2 >= point) ++ getIntervals(point, tree.right)
          } else { // Take all the intervals overlapping with the centerPoint
            tree.overlappingLeftSorted
          }
        }
      }

    getIntervals(point, Some(this))
  }

  def getOverlappingFromInterval(interval: Interval): List[Interval] = {
    if (interval._2 <= this.centerPoint) getOverlappingFromPoint(interval._2)
    else if (interval._1 >= this.centerPoint) getOverlappingFromPoint(interval._1)
    else {
      def getIntervals(interval: Interval, tree: Option[IntervalTree]): List[Interval] =
        tree match {
          case None => List()
          case Some(tree) => {
            if (interval._1 < tree.centerPoint) {
              tree.overlappingRightSorted.takeWhile(_._2 <= interval._2) ++ getIntervals(interval, tree.left)
            } else if (interval._2 > tree.centerPoint) {
              tree.overlappingLeftSorted.takeWhile(_._1 >= interval._1) ++ getIntervals(interval, tree.right)
            } else {
              tree.overlappingLeftSorted
            }
          }
        }

      getIntervals(interval, Some(this))
    }
  }
}

object IntervalTree {

  type Point = Int
  type Interval = (Point, Point)

  def apply(intervals: List[Interval]): Option[IntervalTree] = {

    def createIntervalTree: List[Interval] => Option[IntervalTree] =
      _ match {
        case List() => None
        case intervals => {
          val min = intervals.minBy(_._1)._1
          val max = intervals.maxBy(_._2)._2
          val centerPoint = Math.ceil((max - min) / 2).toInt
          val left = this.apply(intervals.filter(_._2 < centerPoint))
          val right = this.apply(intervals.filter(_._1 > centerPoint))
          val middle = intervals.filter(tuple => tuple._1 <= centerPoint && tuple._2 >= centerPoint)
          val overlappingLeftSorted = middle.sortBy(_._1)
          val overlappingRightSorted = middle.sortBy(_._2)
          Some(IntervalTree(centerPoint, left, right, overlappingLeftSorted, overlappingRightSorted))
        }
      }

    createIntervalTree(intervals)
  }
}

object IntervalTreeTest {

  def main(args: Array[String]): Unit = {
    val tree = IntervalTree(List((1,8), (7, 20), (4, 12))).get
    assert(tree.getOverlappingFromPoint(2) == List((1, 8)))
    assert(tree.getOverlappingFromPoint(8) == List((4, 12), (7, 20), (1, 8)))
    assert(tree.getOverlappingFromPoint(6) == List((4, 12), (1, 8)))
    assert(tree.getOverlappingFromInterval((12, 20)) == List((4, 12), (7, 20)))
    assert(tree.getOverlappingFromInterval((9, 9)) == List((4, 12), (7, 20)))
    assert(tree.getOverlappingFromInterval((1, 1)) == List((1, 8)))
    assert(tree.getOverlappingFromInterval((1, 2)) == List((1, 8)))
    assert(tree.getOverlappingFromInterval((1, 200)) == List((4, 12), (7, 20), (1, 8)))
  }
}
