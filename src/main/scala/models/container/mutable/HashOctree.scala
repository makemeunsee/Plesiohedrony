package models.container.mutable

import models.container.Octree
import models.container.Boundable
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import Octree._

class MOctreeLeaf[T <: Boundable](
    val center: (Int, Int, Int),
    val values: Set[T] = new HashSet[T]
) extends Octree[T] {

  val depth = maxDepth

  def valuesAt(boundable: Boundable) =
    if (boundable.within(bounds))
      values
    else
      new HashSet[T]

  def addValue(t: T) = {
    if (contains(t) && !values.contains(t))
      values += t
    this
  }

  def removeValue(t: T) = {
    if (contains(t) && values.contains(t))
      values -= t
    this
  }

  def empty = values.isEmpty

  override def toString = s"mutable.Leaf $bounds"
}

class MOctreeNode[T <: Boundable](val center: (Int, Int, Int), val depth: Int)
    extends Octree[T] {

  var vChildren: Option[Seq[Octree[T]]] = None
  override def children = vChildren

  def values = children match {
    case Some(kids) => kids.map(_.values).reduce(_ ++ _)
    case _          => new HashSet[T]
  }

  def valuesAt(boundable: Boundable) =
    if (boundable.within(bounds))
      children match {
        case Some(kids) => kids.map(_.valuesAt(boundable)).reduce(_ ++ _)
        case _          => new HashSet[T]
      }
    else
      new HashSet[T]

  def addValue(t: T) = {
    if (contains(t))
      children match {
        case Some(kids) => kids.foreach(_.addValue(t))
        case _ =>
          vChildren = Some(createChildren[T](center, depth).map(_.addValue(t)))
      }
    this
  }

  def removeValue(t: T): Octree[T] = {
    if (contains(t))
      children match {
        case Some(kids) => {
          kids.map(_.removeValue(t))
          for (kid <- kids)
            if (!kid.empty)
              return this
          vChildren = None
        }
        case _ => this
      }
    this
  }

  def empty: Boolean = children match {
    case Some(kids) => {
      for (kid <- kids)
        if (!kid.empty)
          return false
      true
    }
    case _ => true
  }

  private def createChildren[T <: Boundable](
      center: (Int, Int, Int),
      depth: Int
  ): Seq[Octree[T]] = {
    def creator(childCenter: (Int, Int, Int)): Octree[T] = {
      val childDepth = depth + 1
      if (childDepth < maxDepth)
        new MOctreeNode[T](childCenter, childDepth)
      else if (childDepth == maxDepth)
        new MOctreeLeaf[T](childCenter)
      else
        throw new Error(
          s"requesting octree depth $childDepth > to max depth $maxDepth"
        )
    }
    val diff = width / 4
    List(
      creator((center._1 - diff, center._2 - diff, center._3 - diff)),
      creator((center._1 - diff, center._2 - diff, center._3 + diff)),
      creator((center._1 - diff, center._2 + diff, center._3 - diff)),
      creator((center._1 - diff, center._2 + diff, center._3 + diff)),
      creator((center._1 + diff, center._2 - diff, center._3 - diff)),
      creator((center._1 + diff, center._2 - diff, center._3 + diff)),
      creator((center._1 + diff, center._2 + diff, center._3 - diff)),
      creator((center._1 + diff, center._2 + diff, center._3 + diff))
    )
  }

  override def toString = s"mutable.Node $bounds"
}
