package models

package object container {
  type Bounds = (Int, Int, Int, Int, Int, Int)
  
  trait Boundable {
    def within(bounds: Bounds): Boolean
    def joinedNeighborhood(neighbors: Set[_ <: Boundable])
    def leftNeighborhood(neighbors: Set[_ <: Boundable])
  }
  
  def pointWithin(bounds: Bounds, p: Point3f) =
    p.x >= bounds._1 &&
    p.x <= bounds._2 &&
    p.y >= bounds._3 &&
    p.y <= bounds._4 &&
    p.z >= bounds._5 &&
    p.z <= bounds._6    
}