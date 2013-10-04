package engine.entity

import engine.{Camera, Scene}
import models.container.{Boundable, Bounds, Octree}
import scala.math.{Pi}
import scala.collection.mutable.HashSet
import scala.collection.immutable.Set
import models.Point3f
import engine.rendering.FaceRenderable
import engine.Element

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
  
  val colliding = new HashSet[Element]
    
  import util.Collections.successivePairsCycling
  private def pointInsideFace[T <: FaceRenderable](face: T, p: Point3f): Boolean = {
    successivePairsCycling(face.toContour.toList).forall { case (pA, pB) =>
      // for each edge AB, check direction of AB ^ AP to know on which side of AB lies P
      ((pB - pA) ^ (p - pA)) * face.normal > 0
    }
  }
  
  private def circleIntersectsFaceEdge[T <: FaceRenderable](face: T, pO: Point3f, r: Float): Boolean = {
    successivePairsCycling(face.toContour.toList).exists{ case (pA, pB) =>
       // compute distance between circle center O and line AB
      val vAO = pO - pA
      val vBO = pO - pB
      // either point in inside the circle
      vAO * vAO < r * r ||
      vBO * vBO < r * r || {
        // or the projection of the circle center is inside the segment
        val vAB = (pB - pA)
        val u = vAB.normalize
        val d = (vAO ^ u).norm
        // not too far to project
        d < r && {
          // compute projection P of center O on AB
          val dAP = vAO * u
          val pP = pA + (u * dAP)
          // P must be between A and B
          (pA - pP) * (pB - pP) < 0
        }
      }
    }
  }
  
  def collide[T <: FaceRenderable](position: Point3f, suspects: Set[T]): Set[T] = perf.Perf.perfed("collide") {
    // center to center vector
    suspects.filter{ face =>
      val n = face.normal
      val fCenter = face.center
      // player center to center of the face
      val vec = position - fCenter
      // project along normal => distance between face and player center
      val d = math.abs(vec * n)
      // bounding sphere must at least intersects the face plane
      d * d < squareRadius && {
        // distance is not enough, check if projection inside face
        
        // the bounding sphere of the player intersects the face plane, forming a circle
        // center of the circle of intersection
        val pCenter = position - (n * d)
        // radius of the circle of intersection
        val r = math.sqrt(squareRadius - d*d).toFloat
        
        // intersection circle is inside the face polygon or
        // it intersects an edge of the face polygon
        pointInsideFace(face, pCenter) || circleIntersectsFaceEdge(face, pCenter, r)
      }
    }
  }
  
  override def setXYZ(newX: Float, newY: Float, newZ: Float) = perf.Perf.perfed("move player") {
    if (getX != newX || getY != newY || getZ != newZ) {
      val neighbors = scene.octree.valuesAt(this)
      colliding.clear
      colliding ++= collide(new Point3f(newX, newY, newZ), neighbors)
      if ( colliding.isEmpty)
        super.setXYZ(newX, newY, newZ)
      else {
        //println(s"colliding: ${colliding.mkString(", ")}")
        val allowed = colliding.map(_.normal).foldLeft(new Point3f(newX-getX, newY-getY, newZ-getZ)) { case (acc,n) =>
          val nComponent = acc * n
          if ( nComponent < 0 )
            acc - (n * (acc * n) )
          else
            acc
        }
        super.setXYZ(getX + allowed.x, getY + allowed.y, getZ + allowed.z)
      }
    }
  }
  
  override def toString = s"Player @ $getX - $getY - $getZ"
}