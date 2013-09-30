package sandbox

import models.Honeycomb
import Honeycomb.{Point3f, Face, Polyhedron}
import engine.rendering.Renderer.{Growable, ID}
import perf.Perf.perfed

object DefaultGrowable {
  
  def polyhedronToGrowables(honeycomb: Honeycomb)(poly: Polyhedron): Iterable[Growable] = perfed("polyhedronToGrowables") {
    poly.faces.map( face => perfed("growable") {
      new DefaultGrowable(poly.i,
        poly.j,
        poly.k,
        poly.id,
        face,
        poly.origin,
        poly.center,
        honeycomb)
    })
  }
  
  import org.lwjgl.opengl.GL11.{glVertex3f}
  
  def drawTriangle(t: (Point3f, Point3f, Point3f)) {
    glVertex3f(t._1.x, t._1.y, t._1.z)
    glVertex3f(t._2.x, t._2.y, t._2.z)
    glVertex3f(t._3.x, t._3.y, t._3.z)
  }

  def successivePairsCycling[A](list: List[A]): List[(A, A)] = {
    def successivePairsCyclingRec(rem: List[A], first: A): List[(A, A)] =
      rem match {
        case List(last) => List((last, first))
        case h1 :: h2 :: t => {
          (h1, h2) :: successivePairsCyclingRec(h2 :: t, first)
        }
      }
    list match {
      case List()  => List()
      case List(_) => List()
      case h1 :: h2 :: t => {
        (h1, h2) :: successivePairsCyclingRec(h2 :: t, h1)
      }
    }
  }

  def drawClosedContour(pts: Iterable[Point3f]) {
    successivePairsCycling(pts.toList).map{ case (p1, p2) =>
      glVertex3f(p1.x, p1.y, p1.z)
      glVertex3f(p2.x, p2.y, p2.z)
    }
  }
  
}
import DefaultGrowable._

class DefaultGrowable(i: Int, j: Int, k: Int, polyId: Int, face: Face, origin: Point3f, val center: Point3f, honeycomb: Honeycomb) extends Growable {
  def id = (i, j, k, face.id)

  val positionedPolygon = face.withOrigin(origin)
  def drawables = positionedPolygon.toTriangles
  def contour = positionedPolygon.toContour

  def growth: Iterable[Growable] = {
    val opposite = face.opposite(i, j, k)
    val neighPoly = honeycomb.polyhedron(opposite.i, opposite.j, opposite.k, opposite.face.polyId)
    polyhedronToGrowables(honeycomb)(neighPoly)
  }

  def trunk: Iterable[ID] = perfed("trunk") { honeycomb.polyhedron(i,j,k,polyId).faces.map(f => (i, j, k, f.id)) }
  
  def touching = perfed("touching") {
    val faceId = face.opposite(i, j, k)
    (faceId.i, faceId.j, faceId.k, faceId.face.id)
  }

  import engine.Math.%+
  // TODO outsource default growable and color management, keep only picking stuff here
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
  def renderContour = drawClosedContour(contour)

  def render = renderWithColor((%+(polyId, 2), %+(k + j + i, 2)) match {
    case (0,0) => (-1,100,0) // orange
    case (1,0) => (100,0,-96) // violet
    case (0,1) => (100,0,-96) // violet
    case (1,1) => (-1,100,0) // orange
  })
  
  import engine.rendering.Picking.Color3B
    
  def renderPicking(c: Color3B) = renderWithColor(c)
  
  def renderPicked(pickedColor: Color3B) = renderWithColor(pickedColor)
  
  import org.lwjgl.opengl.GL11.{glColor3ub, glNormal3f}
  
  private def renderWithColor(c: Color3B) {
    glColor3ub(c._1, c._2, c._3)
    val normal = face.polygon.normal
    glNormal3f(normal.x, normal.y, normal.z)
    drawables.map(drawTriangle)
  }
}