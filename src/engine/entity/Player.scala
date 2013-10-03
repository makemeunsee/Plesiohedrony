package engine.entity

import engine.{Camera, Scene}
import models.container.{Boundable, Bounds, Octree}
import scala.math.{Pi}
import scala.collection.mutable.HashSet
import scala.collection.immutable.Set
import models.Point3f
import engine.rendering.FaceRenderable

class Player(scene: Scene) extends Camera(Pi / 2d,0,0,0,0) with Boundable {

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
  
  // TODO not yet working...
  def collide[T <: FaceRenderable](position: Point3f, suspects: Set[T]): Set[T] = {
    // center to center vector
    suspects.filter{ face =>
      val n = face.normal
      // player center to some point of the suspect face
      val vec = position - face.toContour.head
      // project along normal => distance between face and player center
      val d = vec * n
      // bounding sphere must at least intersects the face plane
      d * d < squareRadius && {
        // distance is not enough, check if projection inside face
        val projectedPoint = position - (n * d)
        // TODO wrong point, must use the point of the circle of intersection between
        // the player bounding sphere and the face plane which is the closest
        // to the face center (ok because face is convex)
        import util.Collections.successivePairsCycling
        // for each edge AB, check direction of AB ^ AP where P is the projected point
        successivePairsCycling(face.toContour.toList).foldLeft(true) { case (acc, (pA, pB)) =>
          acc && ((pB - pA) ^ (projectedPoint - pA)) * n > 0
        }
      }
    }
  }
  
  override def setXYZ(newX: Float, newY: Float, newZ: Float) = perf.Perf.perfed("move player") {
    if (getX != newX || getY != newY || getZ != newZ) {
      val neighbors = scene.octree.valuesAt(this)
      println(s"neighbors: ${neighbors.mkString(" ,")}")
      //println(s"colliding: ${collide(new Point3f(newX, newY, newZ), neighbors).mkString(" ,")}")
      super.setXYZ(newX, newY, newZ)
    }
  }
  
  override def toString = s"Player @ $getX - $getY - $getZ"
}