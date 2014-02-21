package engine

import models.container.Boundable

package object entity {
  
  trait Entity extends Boundable {
    def getX: Float
    def getY: Float
    def getZ: Float
  }
  
  trait SphereEntity extends Entity {
    def radius: Float
  }
  
}