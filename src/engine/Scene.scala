package engine

import scala.collection.mutable.ArrayBuffer
import engine.Ticker.Tickable
import rendering.{Growable, Renderable, SpecialRenderable, ID}
import collection.mutable
import perf.Perf.perfed
import models.container.{Boundable, Bounds, Octree}
import engine.entity.Player
import models.container.immutable.OctreeNode
import models.container.immutable.MOctreeNode

trait Element extends Boundable with Growable[Element]

class Scene(hideTouching: Boolean) extends Tickable {

  var octree: Octree[Element] = new MOctreeNode[Element]((0,0,0), 0)
  
  val player = new Player(this)
  val camera = player

  val wireframes = new ArrayBuffer[Renderable]
  private val elements = new mutable.HashMap[ID, Element]
  val visible = new mutable.HashMap[ID, Element]
  val translationlessRenderables = new mutable.HashSet[SpecialRenderable]

  def addWireframe(drawable: Renderable) {
    wireframes += drawable
  }
  
  def addElement(e: Element) = perfed("addElement") {
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
  
  def removeElement(id: ID) = perfed("removeElement") {
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
  
  val growChance = 1.3
  val decayChance = 1
  val lifeRate = 10000
  
  def tick(tick: Int) {
    val mod = tick % 5
    if( mod == 0 ) {
      for ( e <- visible.values )
        if ( math.random * lifeRate < growChance )
          for ( g <- e.growth )
            addElement(g)
    } else if ( mod == 4 ) {
      for ( e <- elements.values )
        if ( math.random * lifeRate < decayChance )
          for ( id <- e.trunk )
            removeElement(id)
    }
  }
}