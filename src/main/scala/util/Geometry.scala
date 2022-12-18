package util

object Geometry {
  def cubeBounds[T](center: (T, T, T), cubeHalfWidth: T)(implicit num: Numeric[T]): (T, T, T, T, T, T) = {
    import num._
    (center._1 - cubeHalfWidth,
    center._1 + cubeHalfWidth,
    center._2 - cubeHalfWidth,
    center._2 + cubeHalfWidth,
    center._3 - cubeHalfWidth,
    center._3 + cubeHalfWidth)
  }
  
  import models.Point3f
  def boundingCube[T](center: (T, T, T), cubeHalfWidth: T)(implicit num: Numeric[T]): List[List[Point3f]] = {
    implicit def numToFloat(t: T): Float = num.toFloat(t)
    val bounds = cubeBounds(center, cubeHalfWidth)
    List(
      List(new Point3f(bounds._1, bounds._3, bounds._5), new Point3f(bounds._1, bounds._4, bounds._5), new Point3f(bounds._1, bounds._4, bounds._6), new Point3f(bounds._1, bounds._3, bounds._6)).reverse,
      List(new Point3f(bounds._2, bounds._3, bounds._5), new Point3f(bounds._2, bounds._4, bounds._5), new Point3f(bounds._2, bounds._4, bounds._6), new Point3f(bounds._2, bounds._3, bounds._6)),
      List(new Point3f(bounds._1, bounds._3, bounds._5), new Point3f(bounds._2, bounds._3, bounds._5), new Point3f(bounds._2, bounds._3, bounds._6), new Point3f(bounds._1, bounds._3, bounds._6)),
      List(new Point3f(bounds._1, bounds._4, bounds._5), new Point3f(bounds._2, bounds._4, bounds._5), new Point3f(bounds._2, bounds._4, bounds._6), new Point3f(bounds._1, bounds._4, bounds._6)).reverse,
      List(new Point3f(bounds._1, bounds._3, bounds._5), new Point3f(bounds._1, bounds._4, bounds._5), new Point3f(bounds._2, bounds._4, bounds._5), new Point3f(bounds._2, bounds._3, bounds._5)),
      List(new Point3f(bounds._1, bounds._3, bounds._6), new Point3f(bounds._1, bounds._4, bounds._6), new Point3f(bounds._2, bounds._4, bounds._6), new Point3f(bounds._2, bounds._3, bounds._6)).reverse)
  }
}