package models

trait Honeycomb {
  import Honeycomb.{Polyhedron}
  
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

  private val nextIndex: (() => Int) = {
    var index = -1
    () => { index += 1; index }
  }

  abstract case class Face(val polyId: Int, val polygon: Polygon) {
    val id = nextIndex()
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