package engine

import models.Point3f

package object rendering {
  // coordinates + face id
  type ID = (Int, Int, Int, Int)

  type Color3B = (Byte, Byte, Byte)

  val BLACK: Color3B = (0, 0, 0)
  val RED: Color3B = (-1, 0, 0)
  val YELLOW: Color3B = (-1, -1, 0)
  val ORANGE: Color3B = (-1, 127, 0)
  val ORANGE2: Color3B = (127, -1, 0)
  val GREEN: Color3B = (0, -1, 0)
  val CYAN: Color3B = (-1, 0, -1)
  val CYAN2: Color3B = (-1, 0, 127)
  val CYAN3: Color3B = (127, 0, -1)
  val BLUE: Color3B = (-1, 0, 0)
  val VIOLET: Color3B = (-1, 0, -1)
  val VIOLET2: Color3B = (-1, 0, 127)
  val VIOLET3: Color3B = (127, 0, -1)
  val WHITE: Color3B = (-1, -1, -1)
  val GREY1: Color3B = (127, 127, 127)
  val GREY2: Color3B = (85, 85, 85)
  val GREY3: Color3B = (-86, -86, -86)

  val COLORS: Seq[Color3B] = Seq(BLACK, RED, YELLOW, ORANGE, ORANGE2, GREEN, CYAN, CYAN2, CYAN3, BLUE, VIOLET, VIOLET2,
    VIOLET3, WHITE, GREY1, GREY2, GREY3)

  trait Renderable {
    def color: Color3B
    def toTriangles: Iterable[(Point3f, Point3f, Point3f)]
    def toContour: Iterable[Point3f]
    def normal: Point3f
  }
  
  trait SpecialRenderable {
    def render()
  }

  // convex polygonal face
  trait FaceRenderable extends Renderable {
    // which other triangle this is touching
    def touching: ID
    def center: Point3f
    def polyhedronCenter: Point3f
    def distanceToPolyhedronCenter: Float
  }

  trait Pickable extends FaceRenderable {
    def pickedColor: Color3B
    def id: ID
  }

  trait ColorMutation {
    def setColor(color: Color3B): Unit
  }

  // a renderable face (triangle)
  trait Growable[T <: Growable[T]] extends Pickable {
    // the new set of growables obtainable from this
    def growth: Iterable[T]
    // the set of growables this is part of
    def trunk: Iterable[ID]
  }
}