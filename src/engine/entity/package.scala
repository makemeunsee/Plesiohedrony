package engine

import models.container.Boundable

package object entity {
  trait MovingListener {
    def aboutToMove(src: Moving)
    def moved(src: Moving)
  }

  trait Moving {
    def addMovingListener(ml: MovingListener)
    def removeMovingListener(ml: MovingListener)
  }
}