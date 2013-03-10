package org.bertails

import scala.annotation.tailrec
import scala.collection.mutable.DoubleLinkedList

/** Input: a sequence of buildings, defined by their heights.
  * Problem: as it's raining, how much water accumulates?
  * Rule: the water can accumulate only between two buildings.
  */
object WaterAccumulation {

  /** A building, defined by its height and witdh (width defaults to 1) */
  case class Building(height: Int, width: Int = 1)

  /** Each building is a node in a double linked list */
  type Node = DoubleLinkedList[Building]

  /** Encodes the problem as a double linked list of buildings of width 1
    * 
    * @param buildings the heights of the buildings
    */
  def Buildings(buildings: Int*): Node = DoubleLinkedList(buildings.map(Building(_)): _*)

  /* each rule is encoded as an extractor, which encapsulates the logic
   * and its effect, if applicable
   * remark: the order of application of the rules matters
   */

  /** if the building is a local minimum, the problem is the same as if
    * the building were taller (the new height matches the one of the
    * smallest building). We just need to return the amount of
    * accumulated water above this building. */
  object LocalMinimum {

    def isLocalMinimum(node: Node): Boolean =
      node.prev != null &&
        node.next.nonEmpty &&
        node.elem.height < node.prev.elem.height &&
        node.elem.height < node.next.elem.height

    def unapply(node: Node): Option[(Node, Int)] =
      if (isLocalMinimum(node)) {
        val min = math.min(node.prev.elem.height, node.next.elem.height)
        val diff = min - node.elem.height
        node.elem = node.elem.copy(height = min)
        Some((node, diff * node.elem.width))
      } else {
        None
      }

  }

  /** if the building is on the far left and cannot contribute to
    * accumulate water, we just remove it
    */
  object MinLeft {

    def isMinLeft(node: Node): Boolean =
      node.prev == null &&
        node.next.nonEmpty &&
        node.elem.height < node.next.elem.height
  
    def unapply(node: Node): Option[Node] =
      if (isMinLeft(node)) {
        node.remove()
        Some(node.next)
      } else {
        None
      }

  }

  /** if the building is on the far right and cannot contribute to
    * accumulate water, we just remove it
    */
  object MinRight {

    def isMinRight(node: Node): Boolean =
      node.next.isEmpty &&
        node.prev != null &&
        node.elem.height < node.prev.elem.height
  
    def unapply(node: Node): Option[Node] =
      if (isMinRight(node)) {
        node.remove()
        Some(node.prev)
      } else {
        None
      }

  }

  /** that's the trick to this problem: two buildings of the same height
    * are equivalent to one new building with the accumulated
    * width. By doing that, we make it easy to see a local minimum.
    */
  object Collapsable {

    def canBeCollapsed(node: Node): Boolean =
      node.prev != null &&
        node.elem.height == node.prev.elem.height

    def unapply(node: Node): Option[Node] =
      if (canBeCollapsed(node)) {
        node.elem = node.elem.copy(width = node.elem.width + node.prev.elem.width)
        node.prev.remove()
        Some(node)
      } else {
        None
      }

  }

  object NoBuilding {

    def unapply(node: Node): Boolean =
      node.prev == null && node.isEmpty

  }

  object SingleBuilding {

    def unapply(node: Node): Boolean =
      node.prev == null && node.next.isEmpty

  }

  /** that's the default rule: if nothing is applicable, just move to
    * the next building.
    */
  object MoveRight {

    def unapply(node: Node): Option[Node] =
      if (node.nonEmpty) Some(node.next) else None

  }

  /** returns how much water accumulates */
  def fillWithWater(node: Node): Int = {
    /** Auxiliary function that is called recursively for each step.
      * This terminates as we always do one of the following:
      *  1. move to right (there is finite number of building)
      *  2. remove buildings
      *  3. change a building height such that it can immediatly be collapsed
      */
    @tailrec def fill(
      /** a pointer to current building in the list (reminder: a DoubleLinkedList is mutable) */
      node: Node,
      /** the amount of water accumulated so far */
      water: Int): Int = {
      node match {
        case NoBuilding() | SingleBuilding() => water
        case MinLeft(n) => fill(n, water)
        case MinRight(n) => fill(n, water)
        case LocalMinimum(n, diff) => fill(n, water + diff)
        case Collapsable(n) => fill(n, water)
        case MoveRight(n) => fill(n, water)
        case _ => sys.error(s"unexpected configuration $node")
      }
    }
    fill(node, 0)
  }

  /** Executes some tests */
  def main(args: Array[String]): Unit = {
    assert(fillWithWater(Buildings()) == 0)
    assert(fillWithWater(Buildings(5)) == 0)
    assert(fillWithWater(Buildings(5, 5)) == 0)
    assert(fillWithWater(Buildings(5, 2, 2, 5)) == 6)
    assert(fillWithWater(Buildings(5, 2, 5, 2, 5)) == 6)
    assert(fillWithWater(Buildings(5, 2, 5, 2, 5, 10)) == 6)
    assert(fillWithWater(Buildings(1, 5, 2, 5, 2, 5, 10, 3, 5)) == 8)
    assert(fillWithWater(Buildings(1, 5, 2, 5, 2, 5, 10, 3, 3, 3, 5)) == 12)
    assert(fillWithWater(Buildings(5, 4, 4, 3, 3, 2, 2, 1, 1, 5)) == 20)
    assert(fillWithWater(Buildings(5, 1, 1, 2, 2, 3, 3, 4, 4, 5)) == 20)
  }

}
