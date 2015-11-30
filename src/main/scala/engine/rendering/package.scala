package engine

import engine.rendering.Colors.Color3B
import models.Point3f

package object rendering {
  // coordinates + face id
  type ID = (Int, Int, Int, Int)

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