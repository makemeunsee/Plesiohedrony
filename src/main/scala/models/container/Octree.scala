package models.container

import scala.math.Numeric
import util.Geometry.{boundingCube, cubeBounds}

object Octree {
  val maxDepth = 15
  // width of a leaf
  val minWidth = 2
  // width of the cube-shaped space covered by an octree
  val width = 2 * math.pow(2, maxDepth).toInt

  def zerokid[T <: Boundable] = List[Octree[T]]()
}
import Octree._

// immutable octree
trait Octree[T <: Boundable] {
  import scala.collection.Set

  def depth: Int
  def center: (Int, Int, Int)
  def bounds: Bounds = cubeBounds(center, width / 2)
  def children: Option[Seq[Octree[T]]] = None
  def values: Set[T]
  def valuesAt(boundable: Boundable): Set[T]
  def addValue(t: T): Octree[T]
  def +(t: T) = addValue(t)
  def removeValue(t: T): Octree[T]
  def -(t: T) = removeValue(t)
  def empty: Boolean
  def contains(t: T) = t.within(bounds)

  def width = {
    def widthAtDepthRec(width: Int, remDepth: Int): Int = remDepth match {
      case 0 => width
      case _ => widthAtDepthRec(width / 2, remDepth - 1)
    }
    widthAtDepthRec(Octree.width, depth)
  }

  def quads = boundingCube(center, width / 2)
}
