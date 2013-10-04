package engine

import models.Point3f

package object rendering {
  // coordinates + face id
  type ID = (Int, Int, Int, Int)

  type Color3B = (Byte, Byte, Byte)

  trait Renderable {
    def color: Color3B
    def toTriangles: Iterable[(Point3f, Point3f, Point3f)]
    def toContour: Iterable[Point3f]
  }
  
  trait SpecialRenderable {
    def render
  }

  // convex polygonal face
  trait FaceRenderable extends Renderable {
    // which other triangle this is touching
    def touching: ID
    def center: Point3f
    def normal: Point3f
  }

  trait Pickable extends FaceRenderable {
    def pickedColor: Color3B
    def id: ID
  }

  // a renderable face (triangle)
  trait Growable[T <: Growable[T]] extends Pickable {
    // the new set of growables obtainable from this
    def growth: Iterable[T]
    // the set of growables this is part of
    def trunk: Iterable[ID]
  }
}