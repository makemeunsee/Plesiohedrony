package models.container

object Octree {
  type Bounds = (Int, Int, Int, Int, Int, Int)

  val maxDepth = 15
  val width = math.pow(2, maxDepth+1).toInt //width of the cube-shaped space covered by an octree
  val minWidth = new OctreeLeaf[Boundable]((0,0,0)).width
}
import Octree._
import collection.immutable

trait Boundable {
  def within(bounds: Bounds): Boolean
}

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

sealed class OctreeLeaf[T <: Boundable](val center: (Int, Int, Int),
                           val values: Set[T] = new immutable.HashSet[T]) extends Octree[T] {

  val depth = maxDepth

  def addValue(t: T) =
    if (contains(t))
      new OctreeLeaf(center, values + t)
    else
      this

  override def toString = s"Leaf $bounds"
}

sealed class OctreeNode[T <: Boundable](val center: (Int, Int, Int),
                           val depth: Int,
                           override val children: Option[Seq[Octree[T]]] = None) extends Octree[T] {

  def values = children match {
    case Some(kids) => kids.foldLeft(new immutable.HashSet[T]()){ (z,e) =>
      z ++ e.values
    }
    case _ => new immutable.HashSet[T]()
  }

  def addValue(t: T) = {
    if (contains(t)) {
      children match {
        case Some(kids) => new OctreeNode(center, depth, Some(kids.map(_.addValue(t))))
        case _          => new OctreeNode(center, depth, Some(createChildren[T](center, depth).map(_.addValue(t))))
      }
    } else
      this
  }

  private def createChildren[T <: Boundable](center: (Int, Int, Int), depth: Int): Seq[Octree[T]] = {
    def creator(childCenter: (Int, Int, Int)): Octree[T] = {
      val childDepth = depth + 1
      if (childDepth < maxDepth)
        new OctreeNode[T](childCenter, childDepth)
      else if (childDepth == maxDepth)
        new OctreeLeaf[T](childCenter)
      else
        throw new Error(s"requesting octree depth $childDepth > to max depth $maxDepth")
    }
    val diff = width / 4
    List(creator((center._1 - diff, center._2 - diff, center._3 - diff)),
      creator((center._1 - diff, center._2 - diff, center._3 + diff)),
      creator((center._1 - diff, center._2 + diff, center._3 - diff)),
      creator((center._1 - diff, center._2 + diff, center._3 + diff)),
      creator((center._1 + diff, center._2 - diff, center._3 - diff)),
      creator((center._1 + diff, center._2 - diff, center._3 + diff)),
      creator((center._1 + diff, center._2 + diff, center._3 - diff)),
      creator((center._1 + diff, center._2 + diff, center._3 + diff)))
  }

  override def toString = s"Node $bounds"
}