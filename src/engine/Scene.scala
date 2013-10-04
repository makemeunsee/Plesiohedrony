package engine

import scala.collection.mutable.ArrayBuffer
import engine.Ticker.Tickable
import rendering.{Growable, Renderable, SpecialRenderable, ID}
import collection.mutable
import perf.Perf.perfed
import models.container.{Boundable, Bounds, Octree}
import models.container.immutable.OctreeNode
import engine.entity.Player

trait Element extends Boundable with Growable[Element]

class Scene(hideTouching: Boolean) extends Tickable {

  var octree: Octree[Element] = new OctreeNode[Element]((0,0,0), 0)
  // TODO test immutable/mutable perf, tidy code
  
  val player = new Player(this)
  val camera = player

  val wireframes = new ArrayBuffer[Renderable]
  private val elements = new mutable.HashMap[ID, Element]
  val visible = new mutable.HashMap[ID, Element]
  val translationlessRenderables = new mutable.HashSet[SpecialRenderable]

  def addWireframe(drawable: Renderable) {
    wireframes += drawable
  }
  
  def addElement(element: Element) = perfed("addElement") {
    addGrowable(element)
  }
  
  def removeElement(element: Element) = perfed("removeElement") {
    removeGrowable(element.id)
  }
  
  private def addGrowable(e: Element) {
    if ( hideTouching ) {
      val touching = elements.get(e.touching)
      // hidden faces are removed from the active scene
      touching match {
        case None    => {
          visible += ((e.id, e))
          octree += e
        }
        case Some(t) => {
          visible -= t.id
          octree -= t
        }
      }
    } else {
      visible += ((e.id, e))
      octree += e
    }
    elements += ((e.id, e))
  }
  
  private def removeGrowable(id: ID) {
    if ( hideTouching ) {
      // revealed faces are made visible
      for ( g <- elements.get(id); t <- elements.get(g.touching) ) {
        visible += ((t.id, t))
        octree += t
      }
    }
    visible -= id
    for ( e <- elements.get(id) )
      octree -= e
    elements -= id
  }
  
  def getElement(id: ID) = elements.get(id)
  
  def tick(tick: Int) {
  }

}