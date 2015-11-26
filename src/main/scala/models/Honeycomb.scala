package models

trait Honeycomb extends Serializable {
  import Honeycomb.Polyhedron
  
  def polyhedron(i: Int, j: Int, k: Int, polyId: Int): Polyhedron
  def polyhedrons(i: Int, j: Int, k: Int): Iterable[Polyhedron]
} 

object Honeycomb {

  trait Coordinates {
    def i: Int; def j: Int; def k: Int
    def origin: Point3f
    // translate points according to coordinates
    protected def position(p: Point3f): Point3f = origin + p
  }
  
  case class FaceId(i: Int, j: Int, k: Int, face: Face) {
    override def toString = s"FaceId($i, $j, $k, ${face.polyId}, ${face.id})"
  }

  abstract case class Face(id: Int, polyId: Int, polygon: Polygon) {
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