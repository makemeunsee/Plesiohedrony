package engine

import models.Point3f
import models.container.Boundable
import models.container.Octree
import models.container.immutable.OctreeNode
import client.Configuration

package object physics {

  trait Collidable[T <: Collidable[T]] extends Boundable {
    def collides(c: T): Boolean
    def position: Point3f
  }

  trait Physics[A <: Collidable[A]] {
    def playerMove(player: A, movement: Point3f): Point3f
  
    def playerCollisions(player: A, playerPosition: Point3f): Iterable[A]
    
    def addWorldObject(o: A)
    
    def removeWorldObject(o: A)
    
    def objectTree: Octree[A]
  }
  
  abstract class AbstractPhysics[A <: Collidable[A]] extends Physics[A] {
  
    var objectTree: Octree[A] = new OctreeNode[A]((0,0,0), 0)
  
    def addWorldObject(o: A) {
      objectTree += o
    }
  
    def removeWorldObject(o: A) {
      objectTree -= o
    }
  
    protected def correctMovement(movement: Point3f, player: A, colliding: A): Point3f
  
    def playerMove(player: A, movement: Point3f): Point3f = {
      val collisions = playerCollisions(player, player.position + movement)
      collisions.foldLeft(movement) { case (acc, colliding) => correctMovement(acc, player, colliding) }.round(2)
    }
  
    def playerCollisions(player: A, playerPosition: Point3f): Iterable[A] = {
      if (Configuration.propCollision) {
        val neighbors = objectTree.valuesAt(player)
        neighbors filter player.collides
      } else {
        Iterable.empty[A]
      }
    }
  
  }

}