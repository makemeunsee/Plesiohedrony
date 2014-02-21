package models

trait Polygon extends Iterable[Point3f] with Serializable {
  def size: Int
  def scaled(scale: Float, center: Point3f = new Point3f(0,0,0)): Polygon
  def translate(vector: Point3f): Polygon
  def pointSymetry(p: Point3f): Polygon
  def center: Point3f
  def reverse: Polygon
  def toTriangles: Iterable[(Point3f, Point3f, Point3f)]
  def toContour: Iterable[Point3f]
  def normal: Point3f
}
  
abstract class PolygonImpl(pts: Iterable[Point3f], override val size: Int) extends Polygon {
  
  def copy(pts: Iterable[Point3f], size: Int): PolygonImpl
  
  protected val points = pts.take(size).toArray
  assert(points.length > 0)
  
  def scaled(scale: Float, center: Point3f) = copy(map(pt => (pt - center) * scale + center), size)
  
  def translate(vector: Point3f) = copy(map(_ + vector), size)
  
  def pointSymetry(p: Point3f) = copy(map(p * 2f - _), size)
  
  def center = foldLeft(new Point3f(0,0,0))((z,e) => z + e / size)
  
  def reverse = copy(points.reverse, size)
  
  def toTriangles: Iterable[(Point3f, Point3f, Point3f)] =
    for ( i <- 1 until points.length-1 ) yield (points(0), points(i), points(i+1))
  
  def toContour = points
  
  override val normal = {
    val (v1, v2) = (points(1) - points(0), points(2) - points(1))
    new Point3f(v1.y * v2.z - v1.z * v2.y,
                v1.z * v2.x - v1.x * v2.z,
                v1.x * v2.y - v1.y * v2.x).normalize
  }
  
  def iterator = points.iterator
}
  
case class Triangle(p1: Point3f, p2: Point3f, p3: Point3f) extends PolygonImpl(List(p1,p2,p3), 3) {
  def copy(pts: Iterable[Point3f], size: Int) = Triangle(pts.head, pts.tail.head, pts.tail.tail.head)
}
  
case class Quad(pts: Iterable[Point3f]) extends PolygonImpl(pts, 4) {
  def copy(pts: Iterable[Point3f], size: Int) = Quad(pts)
}
  
case class Hexad(pts: Iterable[Point3f]) extends PolygonImpl(pts, 6) {
  def copy(pts: Iterable[Point3f], size: Int) = Hexad(pts)
}