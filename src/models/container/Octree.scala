package models.container

object Octree {
  val maxDepth = 15
  // width of a leaf
  val minWidth = 2
  // width of the cube-shaped space covered by an octree
  val width = 2*math.pow(2, maxDepth).toInt
}
import Octree._

// immutable octree
trait Octree[T <: Boundable] {
  def depth: Int
  def center: (Int, Int, Int)
  def bounds: Bounds = {
    val halfWidth = width / 2
    (center._1 - halfWidth, center._1 + halfWidth, center._2 - halfWidth, center._2 + halfWidth, center._3 - halfWidth, center._3 + halfWidth)
  }
  def children: Option[Seq[Octree[T]]] = None
  def values: Set[T]
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

  import models.Point3f
  def quads = List(
    List(new Point3f(bounds._1, bounds._3, bounds._5), new Point3f(bounds._1, bounds._4, bounds._5), new Point3f(bounds._1, bounds._4, bounds._6), new Point3f(bounds._1, bounds._3, bounds._6)),
    List(new Point3f(bounds._2, bounds._3, bounds._5), new Point3f(bounds._2, bounds._4, bounds._5), new Point3f(bounds._2, bounds._4, bounds._6), new Point3f(bounds._2, bounds._3, bounds._6)),
    List(new Point3f(bounds._1, bounds._3, bounds._5), new Point3f(bounds._2, bounds._3, bounds._5), new Point3f(bounds._2, bounds._3, bounds._6), new Point3f(bounds._1, bounds._3, bounds._6)),
    List(new Point3f(bounds._1, bounds._4, bounds._5), new Point3f(bounds._2, bounds._4, bounds._5), new Point3f(bounds._2, bounds._4, bounds._6), new Point3f(bounds._1, bounds._4, bounds._6)),
    List(new Point3f(bounds._1, bounds._3, bounds._5), new Point3f(bounds._1, bounds._4, bounds._5), new Point3f(bounds._2, bounds._4, bounds._5), new Point3f(bounds._2, bounds._3, bounds._5)),
    List(new Point3f(bounds._1, bounds._3, bounds._6), new Point3f(bounds._1, bounds._4, bounds._6), new Point3f(bounds._2, bounds._4, bounds._6), new Point3f(bounds._2, bounds._3, bounds._6))
  )
}