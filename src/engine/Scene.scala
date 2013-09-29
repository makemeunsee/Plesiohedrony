package engine

import org.lwjgl.opengl.{GL11}
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import engine.Ticker.Tickable
import scala.math.{Pi}
import rendering.Renderer._
import scala.Some
import collection.mutable

class Scene extends Tickable {
  
  val camera = new Camera(Pi / 2d,0,0,0,1.8f)

  val wireframes = new ArrayBuffer[Renderable]
  private val growables = new mutable.HashMap[ID, Growable]
  var visible = new immutable.HashMap[ID, Growable]
  val translationlessRenderables = new mutable.HashSet[Renderable]

  def addWireframe(drawable: Renderable) {
    wireframes += drawable
  }
  
  def addGrowable(growable: Growable) {
    val touching = growables.get(growable.touching)
    // hidden faces are noted as such
    touching match {
      case None    => visible += ((growable.id, growable))
      case Some(t) => visible -= t.id ; visible -= growable.id
    }
    growables += ((growable.id, growable))
  }
  
  def removeGrowable(id: ID) {
    // revealed faces are made visible
    for ( g <- growables.get(id); t <- growables.get(g.touching) )
      visible += ((t.id, t))
    growables -= id
    visible -= id
  }
  
  def getGrowable(id: ID) = growables.get(id)

  def tick(tick: Int) {
  }

}