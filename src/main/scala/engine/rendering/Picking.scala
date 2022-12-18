package engine.rendering

import org.lwjgl.opengl.GL11._
import models.{Point3f}
import util.Math.usB2sB
import org.lwjgl.BufferUtils

object Picking {

  val limit = 50f // m², square of the distance within which picking is active

  def filter[T <: Pickable](
      pickables: Iterable[T],
      origin: Point3f
  ): Map[Color3B, T] = perf.Perf.perfed("picking") {
    pickables
      .filter(p => squareDistanceTo(origin, p.center) < limit)
      .zipWithIndex
      .toMap
      .map(e => (intToColor(e._2 + 1), e._1))
  }

  // TODO test

  def readPicking[T <: Pickable, R](
      atPoint: (Int, Int),
      retained: Map[Color3B, T],
      toResult: T => R
  ): Option[R] = {
    val buff = BufferUtils.createByteBuffer(3)
    glReadPixels(atPoint._1, atPoint._2, 1, 1, GL_RGB, GL_UNSIGNED_BYTE, buff)
    val c = (buff.get(0), buff.get(1), buff.get(2))
    retained.get(c).map(toResult)
  }

  private def intToColor(i: Int): Color3B =
    (
      usB2sB(((i & 0xff0000) >> 16)),
      usB2sB((i & 0xff00) >> 8),
      usB2sB(i & 0xff)
    )

  private def squareDistanceTo(center: Point3f, point: Point3f) =
    (point - center).square
}
