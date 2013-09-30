package models

trait Honeycomb {
  import Honeycomb.{Polyhedron}
  
  def polyhedron(i: Int, j: Int, k: Int, polyId: Int): Polyhedron
  def polyhedrons(i: Int, j: Int, k: Int): Iterable[Polyhedron]
} 

object Honeycomb {

  class Point3f(val x: Float, val y: Float, val z: Float) {
    def this(xd: Double, yd: Double, zd: Double) {
      this(xd.toFloat, yd.toFloat, zd.toFloat)
    }
    
    private def arithOp(op: (Float, Float) => Float)(other: Point3f) =
      new Point3f(op(x, other.x), op(y, other.y), op(z, other.z))
    
    def +(other: Point3f) = arithOp((x1, x2) => x1 + x2)(other)
    
    def -(other: Point3f) = arithOp((x1, x2) => x1 - x2)(other)
    
    def *(other: Point3f) = {
      val prod = arithOp((x1, x2) => x1 * x2)(other)
      prod.x + prod.y + prod.z
    }
    
    def square = this * this
    
    def /(div: Float) = new Point3f(x / div, y / div, z / div)

    def *(factor: Float) = new Point3f(x * factor, y * factor, z * factor)

    def normalize = {
      val norm = math.sqrt(x * x + y * y + z * z).toFloat
      new Point3f(x / norm, y / norm, z / norm)
    }
    
    override def toString = s"($x, $y, $z)"
  }
  
  abstract class Polygon(pts: Iterable[Point3f]) extends Iterable[Point3f] {
    protected val points = pts.take(size).toArray
    def size: Int
    def translate(vector: Point3f): Polygon
    def pointSymetry(p: Point3f): Polygon
    def center: Point3f = foldLeft(new Point3f(0,0,0))((z,e) => z + e / size)
    def reverse: Polygon
    def toTriangles: Iterable[(Point3f, Point3f, Point3f)]
    def toContour: Iterable[Point3f] = points
    lazy val normal = {
      val (v1, v2) = toTriangles.head match {
        case (p1, p2, p3) => (p2 - p1, p3 - p2)
      }
      new Point3f(v1.y * v2.z - v1.z * v2.y,
                  v1.z * v2.x - v1.x * v2.z,
                  v1.x * v2.y - v1.y * v2.x).normalize
    }
    def iterator = points.iterator
  }
  
  class Triangle(val p1: Point3f, val p2: Point3f, val p3: Point3f) extends Polygon(List(p1,p2,p3)) {
    override def size = 3
    def translate(vector: Point3f) = new Triangle(p1 + vector, p2 + vector, p3 + vector)
    def toTriangles = Iterable((p1, p2, p3))
    def pointSymetry(p: Point3f) = new Triangle(p - (p1-p),p - (p2-p),p - (p3-p))
    def reverse = new Triangle(p3, p2, p1)
  }
  
  class Quad(pts: Iterable[Point3f]) extends Polygon(pts) {
    override def size = 4
    def translate(vector: Point3f) = new Quad(map(_ + vector))
    def toTriangles = {
      val arr = toArray
      Iterable((arr(0), arr(1), arr(3)), (arr(1), arr(2), arr(3)))
    }
    def pointSymetry(p: Point3f) = new Quad(map(p * 2f - _))
    def reverse = new Quad(toList.reverse)
  }
  
  class Hexad(pts: Iterable[Point3f]) extends Polygon(pts) {
    override def size = 6
    override val center = super.center
    def translate(vector: Point3f) = new Hexad(map(_ + vector))
    def toTriangles = {
      val arr = toArray
      Iterable((center, arr(0), arr(1)),
          (center, arr(1), arr(2)),
          (center, arr(2), arr(3)),
          (center, arr(3), arr(4)),
          (center, arr(4), arr(5)),
          (center, arr(5), arr(0)))
    }
    def pointSymetry(p: Point3f) = new Hexad(map(p * 2f - _))
    def reverse = new Hexad(toList.reverse)
  }
  
  trait Coordinates {
    def i: Int; def j: Int; def k: Int
    def origin: Point3f
    // translate points according to coordinates
    protected def position(p: Point3f): Point3f = origin + p
  }
  
  case class FaceId(i: Int, j: Int, k: Int, face: Face) {
    override def toString = s"FaceId($i, $j, $k, ${face.polyId}, ${face.id})"
  }

  val nextIndex: (() => Int) = {
    var index = -1
    () => { index += 1; index }
  }

  abstract case class Face(val polyId: Int, val polygon: Polygon) {
    val id = nextIndex()
    def opposite(i:Int, j: Int, k: Int): FaceId
    def withOrigin(origin: Point3f): Polygon = polygon.translate(origin)
  }
  
  trait Polyhedron extends Coordinates {
    
    def name = toString
    
    // source of geometry
    def sourcePolyhedron: Polyhedron
    
    def faces: Iterable[Face] = sourcePolyhedron.faces
    
    // id inside the pattern
    def id: Int = sourcePolyhedron.id
        
    lazy val center: Point3f = position(sourcePolyhedron.center)
  }
}