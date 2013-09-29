package sandbox

import models.{Honeycomb, TetraOctaHoneycomb, GyratedTetraOctaHoneycomb, BitruncatedCubicHoneycomb, CubicHoneycomb, TriakisTruncatedTetraHoneycomb}
import Honeycomb.{Polyhedron, Point3f}
import engine.rendering.Renderer.{Growable, Renderable, ID}
import org.lwjgl.opengl.GL11._

object Shapes {
  
  val honeyComb = new BitruncatedCubicHoneycomb
  
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
  
  // full bubble (heavy)
  def heavyBubble = bubble(20, 4f)
  // nice patterns bubble
  def beautyBubble = bubble(30, 2f)
  
  def gridFloor(span: Int) = TetraOctaHoneycomb.Grid.triangles(span).map(
      t => new Renderable {
        val renderables = t.toTriangles

        override def render() {
          glColor3f(0.2f, 0.8f, 0)
          renderables.map(drawTriangle)
        }

        def renderContour = renderables.map(drawTriangle)
      }
    )
    
  def at(i: Int, j: Int, k: Int) =
    for ( poly <- honeyComb.polyhedrons(i,j,k);
      growable <- polyhedronToGrowables(poly))
      yield growable
    
  def floor(span: Int, level: Int) = {
    val limit = span * span + 2
    val doubleSpan = span * 2
    for (i <- -doubleSpan to doubleSpan;
      j <- -doubleSpan to doubleSpan;
      poly <- honeyComb.polyhedrons(i, j, level);
      d = poly.center.x * poly.center.x + poly.center.y * poly.center.y;
      if ( d < limit );
      growable <- polyhedronToGrowables(poly) )
      yield growable
  }
    
  // brute force bubble
  def bubble(size: Int, tolerance: Float) = {
    val squareTol = tolerance * tolerance
    val coordLimit = size *2+2
    val squareSize = size * size
    for ( i <- -coordLimit until coordLimit;
      j <- -coordLimit until coordLimit;
      k <- -coordLimit to coordLimit;
      poly <- honeyComb.polyhedrons(i, j, k);
      d = squareDistanceToOrigin(poly);
      if ( d < squareSize + squareTol && d > squareSize - squareTol);
      growable <- polyhedronToGrowables(poly) )
      yield growable
  }
  
  def dome(size: Int) = {
    val coordLimit = size *2+2
    val squareSize = size * size
    for ( i <- -coordLimit until coordLimit;
      j <- -coordLimit until coordLimit;
      k <- 0 to coordLimit;
      poly <- honeyComb.polyhedrons(i, j, k);
      val d = squareDistanceToOrigin(poly);
      if ( d < squareSize );
      growable <- polyhedronToGrowables(poly) )
      yield growable
  }
  
  def mcChunk =
    for ( i <- 0 until 16;
      j <- 0 until 16;
      k <- -128 until 1;
      poly <- honeyComb.polyhedrons(i, j, k);
      growable <- polyhedronToGrowables(poly) )
      yield growable
  
  def wall(xSize: Int, zSize: Int) =
    for ( i <- 0 until xSize;
      k <- 0 until zSize;
      poly <- honeyComb.polyhedrons(i, 0, k);
      growable <- polyhedronToGrowables(poly) )
      yield growable
  
  private def squareDistanceToOrigin(poly: Polyhedron) = poly.center.x * poly.center.x + poly.center.y * poly.center.y + poly.center.z * poly.center.z
  
  private def polyhedronToGrowables(poly: Polyhedron): Iterable[Growable] = {
    poly.faces.map( face => new Growable {
     
      val id = (poly.i, poly.j, poly.k, face.id)

      val drawables = face.withOrigin(poly.origin).toTriangles
      val contour = face.withOrigin(poly.origin).toContour

      def center = poly.center
    
      def growth: Iterable[Growable] = {
        val opposite = face.opposite(poly.i, poly.j, poly.k)
        val neighPoly = honeyComb.polyhedron(opposite.i, opposite.j, opposite.k, opposite.face.polyId)
        polyhedronToGrowables(neighPoly)
      }
    
      def trunk: Iterable[ID] = poly.faces.map(f => (poly.i, poly.j, poly.k, f.id))
      
      def touching = {
        val faceId = face.opposite(poly.i, poly.j, poly.k)
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

      def render = renderWithColor((%+(poly.id, 2), %+(poly.k + poly.j + poly.i, 2)) match {
        case (0,0) => (-1,100,0) // orange
        case (1,0) => (100,0,-96) // violet
        case (0,1) => (100,0,-96) // violet
        case (1,1) => (-1,100,0) // orange
      })
  
      import engine.rendering.Picking.Color3B
      
      def renderPicking(c: Color3B) = renderWithColor(c)
  
      def renderPicked(pickedColor: Color3B) = renderWithColor(pickedColor)
  
      private def renderWithColor(c: Color3B) {
        glColor3ub(c._1, c._2, c._3)
        val normal = face.polygon.normal
        glNormal3f(normal.x, normal.y, normal.z)
        drawables.map(drawTriangle)
      }
    }  )
  }
}