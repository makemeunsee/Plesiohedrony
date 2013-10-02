package models.container.immutable

import models.container.Octree
import models.container.Boundable
import scala.collection.immutable.HashSet
import Octree._

sealed class OctreeLeaf[T <: Boundable](val center: (Int, Int, Int),
                           val values: Set[T] = new HashSet[T]) extends Octree[T] {

  val depth = maxDepth

  def addValue(t: T) =
    if (contains(t) && !values.contains(t)) {
      //println(s"added $t to $this")
      val newValues = values + t
      t.joinedNeighborhood(values)
      new OctreeLeaf(center, newValues)
    } else
      this
      
  def removeValue(t: T) =
    if (contains(t) && values.contains(t)) {
      val newValues = values + t
      t.leftNeighborhood(values)
      new OctreeLeaf(center, values - t)
    } else
      this
      
  def empty = values.isEmpty

  override def toString = s"Leaf $bounds"
}

sealed class OctreeNode[T <: Boundable](val center: (Int, Int, Int),
                           val depth: Int,
                           override val children: Option[Seq[Octree[T]]] = None) extends Octree[T] {

  def values = children match {
    case Some(kids) => kids.foldLeft(new HashSet[T]()){ (z,e) =>
      z ++ e.values
    }
    case _ => new HashSet[T]()
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
  
  def removeValue(t: T): Octree[T] =
    if (contains(t))
      children match {
        case Some(kids) => {
          val newKids = kids.map(_.removeValue(t))
          for ( newKid <- newKids )
            if (!newKid.empty)
              return new OctreeNode(center, depth, Some(newKids))
          new OctreeNode(center, depth)
        }
        case _          => this
      }
    else
      this
      
  def empty: Boolean = children match {
        case Some(kids) => {
          for ( kid <- kids )
            if (!kid.empty)
              return false
          true
        }
        case _ => true
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