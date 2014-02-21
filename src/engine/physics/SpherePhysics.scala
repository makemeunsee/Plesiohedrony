package engine.physics

import engine.rendering.FaceRenderable
import models.Point3f
import models.container.{Octree, Bounds}
import models.container.immutable.OctreeNode
import scala.collection.Set

trait SphereCollidable extends Collidable[SphereCollidable] {
  def center: Point3f
  def radius: Float
  def collides(c: SphereCollidable) = (c.center - center).norm < (c.radius + radius)
  def position = center
}

class SpherePhysics extends AbstractPhysics[SphereCollidable] {
  
  protected def correctMovement(movement: Point3f, player: SphereCollidable, colliding: SphereCollidable): Point3f = {
    val n = (player.center - colliding.center).normalize
    val nComponent = movement * n
      if ( nComponent < 0 )
        movement - (n * (movement * n) )
      else
        movement
  }
}

object SphereCollidable {
  
  import engine.Element
  import engine.entity.PlayerAvatar
  
  implicit class ElementToSphereCollidable(val e: Element) extends SphereCollidable {
    def center = e.polyhedronCenter
    def radius = e.distanceToPolyhedronCenter
    def within(b: Bounds) = e.within(b)
    override def hashCode = e.hashCode
    override def equals(a: Any) = a match {
      case sc: ElementToSphereCollidable => sc.e == e
      case _ => a == this
    }
  }
  
  import scala.language.implicitConversions
  implicit def playerToSphereCollidable(p: PlayerAvatar): SphereCollidable = new SphereCollidable {
    def center = new Point3f(p.getX, p.getY, p.getZ)
    def radius = p.radius
    def within(b: Bounds) = p.within(b)
  }
}

// legacy code for exact collisions with faces. TODO: move to box collision physics

//  import util.Collections.successivePairsCycling
//  private def pointInsideFace(face: CollidableFace, p: Point3f): Boolean = {
//    successivePairsCycling(face.toContour.toList).forall { case (pA, pB) =>
//      // for each edge AB, check direction of AB ^ AP to know on which side of AB lies P
//      ((pB - pA) ^ (p - pA)) * face.normal > 0
//    }
//  }
//  
//  private def circleIntersectsFaceEdge(face: CollidableFace, pO: Point3f, r: Float): Boolean = {
//    successivePairsCycling(face.toContour.toList).exists{ case (pA, pB) =>
//       // compute distance between circle center O and line AB
//      val vAO = pO - pA
//      val vBO = pO - pB
//      // either point in inside the circle
//      vAO * vAO < r * r ||
//      vBO * vBO < r * r || {
//        // or the projection of the circle center is inside the segment
//        val vAB = (pB - pA)
//        val u = vAB.normalize
//        val d = (vAO ^ u).norm
//        // not too far to project
//        d < r && {
//          // compute projection P of center O on AB
//          val dAP = vAO * u
//          val pP = pA + (u * dAP)
//          // P must be between A and B
//          (pA - pP) * (pB - pP) < 0
//        }
//      }
//    }
//  }
//  
//  private def collide(position: Point3f, suspects: Set[CollidableFace]): Set[CollidableFace] = perf.Perf.perfed("collide") {
//    // center to center vector
//    suspects.filter{ face =>
//      val n = face.normal
//      val fCenter = face.center
//      // player center to center of the face
//      val vec = position - fCenter
//      // project along normal => distance between face and player center
//      val d = math.abs(vec * n)
//      // bounding sphere must at least intersects the face plane
//      d * d < squareRadius && {
//        // distance is not enough, check if projection inside face
//        
//        // the bounding sphere of the player intersects the face plane, forming a circle
//        // center of the circle of intersection
//        val pCenter = position - (n * d)
//        // radius of the circle of intersection
//        val r = math.sqrt(squareRadius - d*d).toFloat
//        
//        // intersection circle is inside the face polygon or
//        // it intersects an edge of the face polygon
//        pointInsideFace(face, pCenter) || circleIntersectsFaceEdge(face, pCenter, r)
//      }
//    }
//  }