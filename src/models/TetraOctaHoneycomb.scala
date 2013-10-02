package models

import util.Math.SQRTS
import SQRTS._
import Honeycomb._
  
class TetraOctaHoneycomb extends Honeycomb {
  
  import TetraOctaHoneycomb.{polyhedronIds, Octahedron, DownwardTetrahedron, UpwardTetrahedron}
  
  def polyhedron(i: Int, j: Int, k: Int, polyId: Int) =
    polyId match {
    case TetraOctaHoneycomb.octaId   => Octahedron(i,j,k)
    case TetraOctaHoneycomb.dTetraId => DownwardTetrahedron(i,j,k)
    case TetraOctaHoneycomb.uTetraId => UpwardTetrahedron(i,j,k)
  }
  
  def polyhedrons(i: Int, j: Int, k: Int) = polyhedronIds.map(id => polyhedron(i,j,k,id))
}

object TetraOctaHoneycomb {
  
  trait TOHCoordinates extends Coordinates {
    def origin: Point3f =
      new Point3f( i + j * 0.5f,
        j * SQRT_3_BY_2 + k * SQRT_3_BY_3,
        k * SQRT_6_BY_3 )
 }
  
  private[models] object Points {
    // TetraOctaHoneycomb pattern points form 2 rhombus, one stacked upon the other, the upper one a bit farther along the y axis
    sealed class TOHPoint(x: Float, y: Float, z: Float) extends Point3f(x,y,z)
    case object North    extends TOHPoint ( 0.5f, SQRT_3_BY_2, 0.0f)
    case object South    extends TOHPoint ( 0.5f, -SQRT_3_BY_2, 0)
    case object East     extends TOHPoint ( 1f, 0f, 0f)
    case object West     extends TOHPoint ( 0f, 0f, 0f)
    case object LowEast  extends TOHPoint ( 1f, -SQRT_3_BY_3, -SQRT_6_BY_3)
    case object LowWest  extends TOHPoint ( 0f, -SQRT_3_BY_3, -SQRT_6_BY_3)
    case object LowNorth extends TOHPoint ( 0.5f, SQRT_3_BY_6, -SQRT_6_BY_3)
    case object LowSouth extends TOHPoint ( 0.5f, -SQRT_3_BY_3-SQRT_3_BY_2, -SQRT_6_BY_3)
  }
  import Points._
    
  // faces for this honeycomb
  
  private[models] object Faces {
    trait TetraOctaFace {
      def id: Int
      def opposite(i: Int, j: Int, k: Int) = id match {
          case O_Top.id       => FaceId(i, j, k+1,   UT_Bottom)
          case O_Bottom.id    => FaceId(i, j, k-1,   DT_Top)
          case O_NorthEast.id => FaceId(i, j+1, k,   UT_SouthWest)
          case O_North.id     => FaceId(i, j, k,     DT_South)
          case O_NorthWest.id => FaceId(i-1, j+1, k, UT_SouthEast)
          case O_SouthWest.id => FaceId(i, j-1, k,   DT_NorthEast)
          case O_South.id     => FaceId(i, j, k,     UT_North)
          case O_SouthEast.id => FaceId(i+1, j-1, k, DT_NorthWest)
          case UT_Bottom.id    => FaceId(i, j, k-1,   O_Top)
          case UT_North.id     => FaceId(i, j, k,     O_South)
          case UT_SouthEast.id => FaceId(i+1, j-1, k, O_NorthWest)
          case UT_SouthWest.id => FaceId(i, j-1, k,   O_NorthEast)
          case DT_Top.id       => FaceId(i, j, k+1,   O_Bottom)
          case DT_NorthWest.id => FaceId(i-1, j+1, k, O_SouthEast)
          case DT_NorthEast.id => FaceId(i, j+1, k,   O_SouthWest)
          case DT_South.id     => FaceId(i, j, k,     O_North)
        }
    }

    // octahedron faces
    object O_Top       extends Face(octaId, new Triangle(South,    East,     West)) with TetraOctaFace
    object O_Bottom    extends Face(octaId, new Triangle(LowEast,  LowWest,  LowNorth)) with TetraOctaFace
    object O_NorthEast extends Face(octaId, new Triangle(LowNorth, East,     LowEast)) with TetraOctaFace
    object O_North     extends Face(octaId, new Triangle(West,     East,     LowNorth)) with TetraOctaFace
    object O_NorthWest extends Face(octaId, new Triangle(West,     LowNorth, LowWest)) with TetraOctaFace
    object O_SouthWest extends Face(octaId, new Triangle(West,     LowWest,  South)) with TetraOctaFace
    object O_South     extends Face(octaId, new Triangle(South,    LowWest,  LowEast)) with TetraOctaFace
    object O_SouthEast extends Face(octaId, new Triangle(East,     South,    LowEast)) with TetraOctaFace
    
    // downward tetrahedron faces
    object DT_Top       extends Face(dTetraId, new Triangle(East,     North,    West)) with TetraOctaFace
    object DT_South     extends Face(dTetraId, new Triangle(East,     West,     LowNorth)) with TetraOctaFace
    object DT_NorthWest extends Face(dTetraId, new Triangle(North,    LowNorth, West)) with TetraOctaFace
    object DT_NorthEast extends Face(dTetraId, new Triangle(North,    East,     LowNorth)) with TetraOctaFace
    
    // upward tetrahedron faces
    object UT_Bottom    extends Face(uTetraId, new Triangle(LowWest,  LowEast,  LowSouth)) with TetraOctaFace
    object UT_North     extends Face(uTetraId, new Triangle(South,    LowEast,  LowWest)) with TetraOctaFace
    object UT_SouthWest extends Face(uTetraId, new Triangle(LowSouth, South,    LowWest)) with TetraOctaFace
    object UT_SouthEast extends Face(uTetraId, new Triangle(LowEast,  South,    LowSouth)) with TetraOctaFace
  }
  import Faces._
    
  case class Octahedron(i: Int, j: Int, k: Int) extends Polyhedron with TOHCoordinates {
    override val sourcePolyhedron = Octahedron0
  }
  
  case class UpwardTetrahedron(i: Int, j: Int, k: Int) extends Polyhedron with TOHCoordinates {
    override val sourcePolyhedron = UpwardTetrahedron0
  }
  
  case class DownwardTetrahedron(i: Int, j: Int, k: Int) extends Polyhedron with TOHCoordinates {
    override val sourcePolyhedron = DownwardTetrahedron0
  }
  
  // index of polyhedron inside the pattern
  private[models] val octaId = 0
  private[models] val uTetraId = 1
  private[models] val dTetraId = 2
  
  val polyhedronIds = List(octaId, uTetraId, dTetraId)
  
  object Octahedron0 extends Octahedron(0 ,0, 0) {
    override val name = "Octahedron0"
    override val id = octaId
    override lazy val faces = List(O_Top, O_Bottom, O_NorthEast, O_North, O_NorthWest, O_SouthWest, O_South, O_SouthEast)
    override lazy val center = faces.foldLeft(new Point3f(0,0,0))((z ,e) => z + e.polygon.center / 8)
  }
  
  object UpwardTetrahedron0 extends UpwardTetrahedron(0 ,0, 0) {
    override val id = uTetraId
    override val name = "UpwardTetrahedron0"
    override lazy val faces = List(UT_Bottom, UT_North, UT_SouthEast, UT_SouthWest)
    override lazy val center = faces.foldLeft(new Point3f(0,0,0))((z ,e) => z + e.polygon.center / 4)
  }
  
  object DownwardTetrahedron0 extends DownwardTetrahedron(0 ,0, 0) {
    override val id = dTetraId
    override val name = "DownwardTetrahedron0"
    override lazy val faces = List(DT_Top, DT_NorthWest, DT_NorthEast, DT_South)
    override lazy val center = faces.foldLeft(new Point3f(0,0,0))((z ,e) => z + e.polygon.center / 4)
  }
  
  object Grid {
    
    sealed abstract class Tile(i: Int, j: Int, src: Polygon) extends TOHCoordinates {
      val k = 0
      val face =  src.translate(origin)
      val center = face.center
    }
    case class Tile_+(i: Int, j: Int) extends Tile(i, j, DT_Top.polygon)
    case class Tile_-(i: Int, j: Int) extends Tile(i, j, O_Top.polygon)

    def triangles(span: Int) = 
      for (i <- -span*2 to span*2;
        j <- -span*2 to span*2;
        tile <- List(Tile_+(i,j), Tile_-(i,j));
        if ( tile.center.x * tile.center.x + tile.center.y * tile.center.y < span*span ))
        yield tile.face
  }
}