package sandbox

import models.{Honeycomb, Point3f}
import Honeycomb.{Face, Polyhedron}
import engine.rendering.Renderer.{Growable, ID}
import engine.rendering.Picking
import Picking.Color3B
import perf.Perf.perfed

object DefaultGrowable {
  
  def polyhedronToGrowables(honeycomb: Honeycomb, scale: Float = 1)(poly: Polyhedron): Iterable[Growable] = perfed("polyhedronToGrowables") {
    poly.faces.map( face => perfed("growable") {
      new DefaultGrowable(poly.i,
        poly.j,
        poly.k,
        poly.id,
        face,
        poly.origin,
        poly.center,
        honeycomb,
        scale)
    })
  }
  
  val DARK_GREY = new Color3B(20,20,20)
  val GREY = new Color3B(-100,-100,-100)
  
  import org.lwjgl.opengl.GL11.{glVertex3f}
}
import DefaultGrowable._

class DefaultGrowable(i: Int, j: Int, k: Int, polyId: Int, face: Face, origin: Point3f, val center: Point3f, honeycomb: Honeycomb, scale: Float) extends Growable {
  def id = (i, j, k, face.id)

  def pickedColor = DARK_GREY
  
  val positionedPolygon = if ( scale != 1f) face.polygon.scaled(scale).translate(origin) else face.withOrigin(origin)
  
  def drawables = positionedPolygon.toTriangles
  
  def contour = positionedPolygon.toContour
    
  def normal = positionedPolygon.normal
  
  def toContour = positionedPolygon.toContour
  
  def toTriangles = positionedPolygon.toTriangles

  def growth: Iterable[Growable] = {
    val opposite = face.opposite(i, j, k)
    val neighPoly = honeycomb.polyhedron(opposite.i, opposite.j, opposite.k, opposite.face.polyId)
    polyhedronToGrowables(honeycomb, scale)(neighPoly)
  }

  def trunk: Iterable[ID] = perfed("trunk") { honeycomb.polyhedron(i,j,k,polyId).faces.map(f => (i, j, k, f.id)) }
  
  def touching = perfed("touching") {
    val faceId = face.opposite(i, j, k)
    (faceId.i, faceId.j, faceId.k, faceId.face.id)
  }

  import engine.Math.%+
  // TODO
//      def render() = renderWithColor((poly.id, %+(poly.k,2)) match {
//        case (0,0) => (-1,100,0) // orange
//        case (1,0) => (-1,0,0) // red
//        case (2,0) => (-1,-1,0) // yellow
////        case (0,1) => (-1,-100,0) // orange
////        case (1,1) => (-1,0,0) // red
////        case (2,1) => (-1,-1,0) // yellow
//        case (0,1) => (100,0,-96) // violet
//        case (1,1) => (-1,0,0) // red
//        case (2,1) => (-1,-1,0) // yellow
//        case _ => (0,0,0) // no pick color
//      })
//      def render() = renderWithColor(%+(poly.k + poly.j + poly.i, 2) match {
//        case 0 => (-1,100,0) // orange
//        case 1 => (100,0,-96) // violet
//      })

  def color = GREY/* ((%+(polyId, 2), %+(k + j + i, 2)) match {
    case (0,0) => (-1,100,0) // orange
    case (1,0) => (100,0,-96) // violet
    case (0,1) => (100,0,-96) // violet
    case (1,1) => (-1,100,0) // orange
  })*/
}