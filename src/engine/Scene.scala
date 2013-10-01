package engine

import scala.collection.mutable.ArrayBuffer
import engine.Ticker.Tickable
import scala.math.{Pi}
import rendering.Renderer._
import collection.mutable
import perf.Perf.perfed
import models.container.{OctreeNode, Boundable, Octree}
import Octree.Bounds

trait Element extends Boundable //with Growable

class Scene(hideTouching: Boolean) extends Tickable {

  var octree: Octree[Element] = new OctreeNode[Element]((0,0,0), 0)
  // TODO remove test code, use octotree with growables or camera, test immutable/mutable perf
  val testElement = new Element {
    def within(b: Bounds) = b._1 <= 15 &&
      b._2 > 15 &&
      b._3 <= 15 &&
      b._4 > 15 &&
      b._5 <= 15 &&
      b._6 > 15

    override def toString = "15,15,15"
  }
  octree += testElement
  println(octree.values)
  // end test code

  val camera = new Camera(Pi / 2d,0,0,0,1.8f)

  val wireframes = new ArrayBuffer[Renderable]
  private val growables = new mutable.HashMap[ID, Growable]
  val visible = new mutable.HashMap[ID, Growable]
  val translationlessRenderables = new mutable.HashSet[SpecialRenderable]

  def addWireframe(drawable: Renderable) {
    wireframes += drawable
  }
  
  def addGrowable(growable: Growable) = perfed("addGrowable") {
    if ( hideTouching ) {
      val touching = growables.get(growable.touching)
      // hidden faces are noted as such
      touching match {
        case None    => visible += ((growable.id, growable))
        case Some(t) => visible -= t.id ; visible -= growable.id
      }
    } else {
      visible += ((growable.id, growable))
    }
    growables += ((growable.id, growable))
  }
  
  def removeGrowable(id: ID) {
    if ( hideTouching ) {
      // revealed faces are made visible
      for ( g <- growables.get(id); t <- growables.get(g.touching) )
        visible += ((t.id, t))
    }
    visible -= id
    growables -= id
  }
  
  def getGrowable(id: ID) = growables.get(id)
  
  def tick(tick: Int) {
  }

}