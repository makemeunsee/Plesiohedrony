package sandbox

import models.{Honeycomb, TetraOctaHoneycomb}
import Honeycomb.{Polyhedron}
import engine.rendering.Renderable
import engine.rendering.Color3B
import DefaultElement.{polyhedronToElements => pTE}

object Shapes {

  val honeyComb =
    Configuration.propHoneycomb.newInstance.asInstanceOf[Honeycomb]

  var scale = 1f

  val polyhedronToElements = pTE(honeyComb, scale)(_)

  // full bubble (heavy)
  def heavyBubble = bubble(20, 4f)
  // nice patterns bubble
  def beautyBubble = bubble(30, 2f)

  val LIGHT_GREEN = new Color3B(50, -50, 0)

  def gridFloor(span: Int) = TetraOctaHoneycomb.Grid
    .triangles(span)
    .map(t =>
      new Renderable {
        override val toTriangles = t.toTriangles
        def toContour = t
        def color = LIGHT_GREEN
      }
    )

  def at(i: Int, j: Int, k: Int) =
    for (
      poly <- honeyComb.polyhedrons(i, j, k);
      growable <- polyhedronToElements(poly)
    )
      yield growable

  def floor(span: Int, level: Int) = {
    val limit = span * span + 2
    val doubleSpan = span * 2
    for (
      i <- -doubleSpan to doubleSpan;
      j <- -doubleSpan to doubleSpan;
      poly <- honeyComb.polyhedrons(i, j, level);
      d = poly.center.x * poly.center.x + poly.center.y * poly.center.y;
      if (d < limit);
      growable <- polyhedronToElements(poly)
    )
      yield growable
  }

  // brute force bubble
  def bubble(size: Int, tolerance: Float) = {
    val squareTol = tolerance * tolerance
    val coordLimit = size * 2 + 2
    val squareSize = size * size
    for (
      i <- -coordLimit until coordLimit;
      j <- -coordLimit until coordLimit;
      k <- -coordLimit to coordLimit;
      poly <- honeyComb.polyhedrons(i, j, k);
      d = squareDistanceToOrigin(poly);
      if (d < squareSize + squareTol && d > squareSize - squareTol);
      growable <- polyhedronToElements(poly)
    )
      yield growable
  }

  def dome(size: Int) = {
    val coordLimit = size * 2 + 2
    val squareSize = size * size
    for (
      i <- -coordLimit until coordLimit;
      j <- -coordLimit until coordLimit;
      k <- 0 to coordLimit;
      poly <- honeyComb.polyhedrons(i, j, k);
      d = squareDistanceToOrigin(poly);
      if (d < squareSize);
      growable <- polyhedronToElements(poly)
    )
      yield growable
  }

  def mcChunk =
    for (
      i <- 0 until 16;
      j <- 0 until 16;
      k <- -128 until 1;
      poly <- honeyComb.polyhedrons(i, j, k);
      growable <- polyhedronToElements(poly)
    )
      yield growable

  def wall(xSize: Int, zSize: Int) =
    for (
      i <- 0 until xSize;
      k <- 0 until zSize;
      poly <- honeyComb.polyhedrons(i, 0, k);
      growable <- polyhedronToElements(poly)
    )
      yield growable

  private def squareDistanceToOrigin(poly: Polyhedron) =
    poly.center.x * poly.center.x + poly.center.y * poly.center.y + poly.center.z * poly.center.z
}
