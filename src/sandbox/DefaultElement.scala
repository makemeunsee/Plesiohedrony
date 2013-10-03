package sandbox

import models.{Honeycomb, Point3f}
import Honeycomb.{Face, Polyhedron}
import engine.rendering.{Growable, ID, Color3B, Picking}
import perf.Perf.perfed
import DefaultElement._
import engine.Element
import models.container.Bounds
import models.container.Boundable

object DefaultElement {
  
  def polyhedronToElements(honeycomb: Honeycomb, scale: Float = 1)(poly: Polyhedron): Iterable[Element] = perfed("polyhedronToElements") {
    poly.faces.map( face => perfed("element") {
      new DefaultElement(poly.i,
        poly.j,
        poly.k,
        poly.id,
        face,
        poly.origin,
        honeycomb,
        scale)
    })
  }
  
  val DARK_GREY = new Color3B(20,20,20)
  val GREY = new Color3B(-100,-100,-100)
  val ORANGE = new Color3B(-1,100,0)
  val VIOLET = new Color3B(100,0,-96)
  
  import org.lwjgl.opengl.GL11.{glVertex3f}
}

class DefaultElement(i: Int, j: Int, k: Int, polyId: Int, face: Face, origin: Point3f, honeycomb: Honeycomb, scale: Float) extends Element {
  def id = (i, j, k, face.id)
  
  //TODO equals and hashcode must be reviewed, they're not consistent when different honeycombs or scales are involved
  override def equals(other: Any): Boolean = other match {
    case elem: Element => id == elem.id
    case _ => false
  }
  
  override def hashCode: Int = id.hashCode
  
  def pickedColor = DARK_GREY
  
  val positionedPolygon = if ( scale != 1f) face.polygon.scaled(scale).translate(origin) else face.withOrigin(origin)
  
  def drawables = positionedPolygon.toTriangles
  
  def contour = positionedPolygon.toContour
    
  def normal = positionedPolygon.normal
  
  def toContour = positionedPolygon.toContour
  
  def toTriangles = positionedPolygon.toTriangles
  
  val center = positionedPolygon.center
  
  def growth: Iterable[Element] = {
    val opposite = face.opposite(i, j, k)
    val neighPoly = honeycomb.polyhedron(opposite.i, opposite.j, opposite.k, opposite.face.polyId)
    polyhedronToElements(honeycomb, scale)(neighPoly)
  }

  def trunk: Iterable[ID] = perfed("trunk") { honeycomb.polyhedron(i,j,k,polyId).faces.map(f => (i, j, k, f.id)) }
  
  def touching = perfed("touching") {
    val faceId = face.opposite(i, j, k)
    (faceId.i, faceId.j, faceId.k, faceId.face.id)
  }

  def color = GREY
  
  private def boundingCube = {
    val bounds = positionedPolygon.foldLeft((Float.MaxValue, Float.MinValue, Float.MaxValue, Float.MinValue, Float.MaxValue, Float.MinValue)) {
      (z, e) => (math.min(z._1, e.x), math.max(z._2, e.x), math.min(z._3, e.y), math.max(z._4, e.y), math.min(z._5, e.z), math.max(z._6, e.z))
    }
    List(new Point3f(bounds._1, bounds._3, bounds._5),
      new Point3f(bounds._1, bounds._4, bounds._5),
      new Point3f(bounds._1, bounds._4, bounds._6),
      new Point3f(bounds._1, bounds._3, bounds._6),
      new Point3f(bounds._2, bounds._3, bounds._5),
      new Point3f(bounds._2, bounds._4, bounds._5),
      new Point3f(bounds._2, bounds._4, bounds._6),
      new Point3f(bounds._2, bounds._3, bounds._6))
  }
  
  def within(bounds: Bounds) = perfed("DefaultElement.within") {
    import models.container.pointWithin
    boundingCube.foldLeft(false) { (z, p) =>
      z || pointWithin(bounds, p)
    }
  }
  
  override def toString = s"$id"
}