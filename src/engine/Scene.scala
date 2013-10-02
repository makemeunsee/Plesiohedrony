package engine

import scala.collection.mutable.ArrayBuffer
import engine.Ticker.Tickable
import rendering.Renderer._
import collection.mutable
import perf.Perf.perfed
import models.container.{Boundable, Bounds, Octree}
import models.container.immutable.OctreeNode
import engine.entity.MovingListener
import engine.entity.{Player, Moving}

trait Element extends Boundable with Growable[Element]

class Scene(hideTouching: Boolean) extends Tickable with MovingListener {

  var octree: Octree[Boundable] = new OctreeNode[Boundable]((0,0,0), 0)
  // TODO test immutable/mutable perf, tidy code
  
  val player = new Player
  player.addMovingListener(this)
  octree += player
  
  val camera = player

  val wireframes = new ArrayBuffer[Renderable]
  private val elements = new mutable.HashMap[ID, Element]
  val visible = new mutable.HashMap[ID, Element]
  val translationlessRenderables = new mutable.HashSet[SpecialRenderable]

  def aboutToMove(src: Moving) {
    octree -= player
  }
  
  def moved(src: Moving) {
    octree += player
  }

  def addWireframe(drawable: Renderable) {
    wireframes += drawable
  }
  
  def addElement(element: Element) = perfed("addElement") {
    addGrowable(element)
    octree += element
  }
  
  def removeElement(element: Element) = perfed("removeElement") {
    removeGrowable(element.id)
    octree -= element
  }
  
  private def addGrowable(growable: Element) {
    if ( hideTouching ) {
      val touching = elements.get(growable.touching)
      // hidden faces are noted as such
      touching match {
        case None    => visible += ((growable.id, growable))
        case Some(t) => visible -= t.id ; visible -= growable.id
      }
    } else {
      visible += ((growable.id, growable))
    }
    elements += ((growable.id, growable))
  }
  
  private def removeGrowable(id: ID) {
    if ( hideTouching ) {
      // revealed faces are made visible
      for ( g <- elements.get(id); t <- elements.get(g.touching) )
        visible += ((t.id, t))
    }
    visible -= id
    elements -= id
  }
  
  def getElement(id: ID) = elements.get(id)
  
  def tick(tick: Int) {
  }

}