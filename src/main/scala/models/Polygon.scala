package models

trait Polygon extends Iterable[Point3f] {
  def size: Int
  def scaled(scale: Float, center: Point3f = new Point3f(0, 0, 0)): Polygon
  def translate(vector: Point3f): Polygon
  def pointSymetry(p: Point3f): Polygon
  def center: Point3f
  def reverse: Polygon
  def toTriangles: Iterable[(Point3f, Point3f, Point3f)]
  def toContour: Iterable[Point3f]
  def normal: Point3f
}

sealed class PolygonImpl(pts: Iterable[Point3f], override val size: Int)
    extends Polygon {

  protected val points = pts.take(size).toArray
  assert(points.length > 0)

  def scaled(scale: Float, center: Point3f) =
    new PolygonImpl(map(pt => (pt - center) * scale + center), size)

  def translate(vector: Point3f) = new PolygonImpl(map(_ + vector), size)

  def pointSymetry(p: Point3f) = new PolygonImpl(map(p * 2f - _), size)

  def center = foldLeft(new Point3f(0, 0, 0))((z, e) => z + e / size)

  def reverse = new PolygonImpl(points.reverse, size)

  def toTriangles: Iterable[(Point3f, Point3f, Point3f)] =
    for (i <- 1 until points.length - 1)
      yield (points(0), points(i), points(i + 1))

  def toContour = points

  override val normal = {
    val (v1, v2) = (points(1) - points(0), points(2) - points(1))
    new Point3f(
      v1.y * v2.z - v1.z * v2.y,
      v1.z * v2.x - v1.x * v2.z,
      v1.x * v2.y - v1.y * v2.x
    ).normalize
  }

  def iterator = points.iterator
}

class Triangle(val p1: Point3f, val p2: Point3f, val p3: Point3f)
    extends PolygonImpl(List(p1, p2, p3), 3)

class Quad(pts: Iterable[Point3f]) extends PolygonImpl(pts, 4)

class Hexad(pts: Iterable[Point3f]) extends PolygonImpl(pts, 6)
