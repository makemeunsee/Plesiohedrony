package engine.entity

import engine.Camera
import models.container.{Boundable, Bounds, Octree}
import scala.math.{Pi}
import scala.collection.mutable.HashSet
import scala.collection.immutable.Set

class Player extends Camera(Pi / 2d,0,0,0,0) with Moving with Boundable {

  val radius = 0.5f
  val squareRadius = radius*radius
  
  // TODO: stretch along z?
  // player bounds is a sphere
  def within(bounds: Bounds) = perf.Perf.perfed("player within") {
    val position = (bounds._1 <= getX, bounds._2 > getX,
      bounds._3 <= getY, bounds._4 > getY,
      bounds._5 <= getZ, bounds._6 > getZ)
    position match {
      case (true, true, true, true, true, true)  => true // fully within
      // closer to a face
      case (false, true, true, true, true, true) => bounds._1 - getX < radius
      case (true, false, true, true, true, true) => getX - bounds._2 < radius
      case (true, true, false, true, true, true) => bounds._3 - getY < radius
      case (true, true, true, false, true, true) => getY - bounds._4 < radius
      case (true, true, true, true, false, true) => bounds._5 - getZ < radius
      case (true, true, true, true, true, false) => getZ - bounds._6 < radius
      // closer to an edge
      case (false, true, false, true, true, true) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._3)*(getY - bounds._3) < squareRadius
      case (false, true, true, false, true, true) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._4)*(getY - bounds._4) < squareRadius
      case (true, false, false, true, true, true) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._3)*(getY - bounds._3) < squareRadius
      case (true, false, true, false, true, true) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._4)*(getY - bounds._4) < squareRadius
      
      case (false, true, true, true, false, true) => (getX - bounds._1)*(getX - bounds._1) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (false, true, true, true, true, false) => (getX - bounds._1)*(getX - bounds._1) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (true, false, true, true, false, true) => (getX - bounds._2)*(getX - bounds._2) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, false, true, true, true, false) => (getX - bounds._2)*(getX - bounds._2) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      
      case (true, true, false, true, false, true) => (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, true, false, true, true, false) => (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (true, true, true, false, false, true) => (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, true, true, false, true, false) => (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      // closer to a vertex
      case (false, true, false, true, false, true) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (false, true, false, true, true, false) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (false, true, true, false, false, true) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (false, true, true, false, true, false) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (true, false, false, true, false, true) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, false, false, true, true, false) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (true, false, true, false, false, true) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, false, true, false, true, false) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      
      case _ => throw new Error(s"Oopps weird: $position")
    }
  }
  
  val neighbors = new HashSet[Boundable]
  
  def joinedNeighborhood(n: Set[_ <: Boundable]) {
    neighbors ++= n
  }  
  
  def leftNeighborhood(n: Set[_ <: Boundable]) {
    neighbors --= n
  }      
    
  private val listeners = new HashSet[MovingListener] 
  def addMovingListener(ml: MovingListener) = listeners += ml
  def removeMovingListener(ml: MovingListener) = listeners -= ml
  private def notifyListeners(notification: (MovingListener => Unit)) = listeners map notification
  
  override def setXYZ(newX: Float, newY: Float, newZ: Float) = perf.Perf.perfed("move player") {
    if (getX != newX || getY != newY || getZ != newZ) {
      notifyListeners({ml: MovingListener => ml.aboutToMove(this)})
      super.setXYZ(newX, newY, newZ)
      notifyListeners({ml: MovingListener => ml.moved(this)})
      println(s"neighbors: ${neighbors.mkString(" ,")}")
    }
  }
  
  override def toString = s"Player @ $getX - $getY - $getZ"
}