package models

import Honeycomb._
import TetraOctaHoneycomb.{TOHCoordinates, Octahedron0, DownwardTetrahedron0, UpwardTetrahedron0}
import util.Math._

class GyratedTetraOctaHoneycomb extends Honeycomb {
  
  import GyratedTetraOctaHoneycomb.{GyratedOctahedron, GyratedDownwardTetrahedron, GyratedUpwardTetrahedron, Octahedron, DownwardTetrahedron, UpwardTetrahedron}
  
  def polyhedron(i: Int, j: Int, k: Int, polyId: Int) =
    (polyId, %+(k,2)) match {
    case (TetraOctaHoneycomb.octaId, 0)   => Octahedron(i,j,k)
    case (TetraOctaHoneycomb.dTetraId, 0) => DownwardTetrahedron(i,j,k)
    case (TetraOctaHoneycomb.uTetraId, 0) => UpwardTetrahedron(i,j,k)
    case (TetraOctaHoneycomb.octaId, 1)   => GyratedOctahedron(i,j,k)
    case (TetraOctaHoneycomb.dTetraId, 1) => GyratedDownwardTetrahedron(i,j,k)
    case (TetraOctaHoneycomb.uTetraId, 1) => GyratedUpwardTetrahedron(i,j,k)
  }

  import TetraOctaHoneycomb.{polyhedronIds}
  def polyhedrons(i: Int, j: Int, k: Int) = polyhedronIds.map(id => polyhedron(i,j,k,id))
}

object GyratedTetraOctaHoneycomb {
  
  trait GTOHPolyhedron extends Polyhedron with TOHCoordinates {
    override def origin = new Point3f(i + j * 0.5f, j * SQRTS.SQRT_3_BY_2, k * SQRTS.SQRT_6_BY_3)
  }
  
  private def ySymmetry(p: Point3f, yPlane: Float): Point3f =
    new Point3f(p.x, yPlane - (p.y - yPlane), p.z)
  
  private def ySymmetry(t: Triangle, yPlane: Float): Triangle =
    new Triangle(ySymmetry(t.p1, yPlane), ySymmetry(t.p2, yPlane), ySymmetry(t.p3, yPlane))
    
  private[models] object GyratedFaces {

    trait GyratedOctaTetraFace {
      def id: Int
      def opposite(i: Int, j: Int, k: Int) = id match {
        case RO_Top.id         => FaceId(i, j, k+1,   GO_Bottom)
        case RO_Bottom.id      => FaceId(i, j, k-1,   GO_Top)
        case RO_NorthEast.id   => FaceId(i, j+1, k,   RUT_SouthWest)
        case RO_North.id       => FaceId(i, j, k,     RDT_South)
        case RO_NorthWest.id   => FaceId(i-1, j+1, k, RUT_SouthEast)
        case RO_SouthWest.id   => FaceId(i, j-1, k,   RDT_NorthEast)
        case RO_South.id       => FaceId(i, j, k,     RUT_North)
        case RO_SouthEast.id   => FaceId(i+1, j-1, k, RDT_NorthWest)
        case RUT_Bottom.id     => FaceId(i, j, k-1,   GDT_Top)
        case RUT_North.id      => FaceId(i, j, k,     RO_South)
        case RUT_SouthEast.id  => FaceId(i+1, j-1, k, RO_NorthWest)
        case RUT_SouthWest.id  => FaceId(i, j-1, k,   RO_NorthEast)
        case RDT_Top.id        => FaceId(i, j, k+1,   GUT_Bottom)
        case RDT_NorthWest.id  => FaceId(i-1, j+1, k, RO_SouthEast)
        case RDT_NorthEast.id  => FaceId(i, j+1, k,   RO_SouthWest)
        case RDT_South.id      => FaceId(i, j, k,     RO_North)
        case GO_Top.id         => FaceId(i, j, k+1,   RO_Bottom)
        case GO_Bottom.id      => FaceId(i, j, k-1,   RO_Top)
        case GO_NorthEast.id   => FaceId(i, j+1, k,   GDT_SouthWest)
        case GO_North.id       => FaceId(i, j, k,     GUT_South)
        case GO_NorthWest.id   => FaceId(i-1, j+1, k, GDT_SouthEast)
        case GO_SouthWest.id   => FaceId(i, j-1, k,   GUT_NorthEast)
        case GO_South.id       => FaceId(i, j, k,     GDT_North)
        case GO_SouthEast.id   => FaceId(i+1, j-1, k, GUT_NorthWest)
        case GUT_Bottom.id     => FaceId(i, j, k-1,   RDT_Top)
        case GUT_South.id      => FaceId(i, j, k,     GO_North)
        case GUT_NorthEast.id  => FaceId(i, j+1, k,   GO_SouthWest)
        case GUT_NorthWest.id  => FaceId(i-1, j+1, k, GO_SouthEast)
        case GDT_Top.id        => FaceId(i, j, k+1,   RUT_Bottom)
        case GDT_SouthWest.id  => FaceId(i, j-1, k,   GO_NorthEast)
        case GDT_SouthEast.id  => FaceId(i+1, j-1, k, GO_NorthWest)
        case GDT_North.id      => FaceId(i, j, k,     GO_South)
      }
    }

    private def toGyratedFace(f: Face) = {
      val t = f.polygon.toTriangles.head
      new Face( f.polyId, ySymmetry(new Triangle(t._1, t._3, t._2), Octahedron0.center.y)) with GyratedOctaTetraFace
    }

    def toRegularFace(f: Face) = new Face( f.polyId, f.polygon) with GyratedOctaTetraFace

    import TetraOctaHoneycomb.Faces._
    // gyrated octahedron faces
    val GO_Top       = toGyratedFace(O_Top)
    val GO_Bottom    = toGyratedFace(O_Bottom)
    val GO_SouthEast = toGyratedFace(O_NorthEast)
    val GO_South     = toGyratedFace(O_North)
    val GO_SouthWest = toGyratedFace(O_NorthWest)
    val GO_NorthWest = toGyratedFace(O_SouthWest)
    val GO_North     = toGyratedFace(O_South)
    val GO_NorthEast = toGyratedFace(O_SouthEast)

    // gyrated downward tetrahedron faces
    val GDT_Top       = toGyratedFace(DT_Top)
    val GDT_North     = toGyratedFace(DT_South)
    val GDT_SouthWest = toGyratedFace(DT_NorthWest)
    val GDT_SouthEast = toGyratedFace(DT_NorthEast)

    // gyrated upward tetrahedron faces
    val GUT_Bottom    = toGyratedFace(UT_Bottom)
    val GUT_South     = toGyratedFace(UT_North)
    val GUT_NorthWest = toGyratedFace(UT_SouthWest)
    val GUT_NorthEast = toGyratedFace(UT_SouthEast)

    // regular faces need to be redefined to use the proper trait

    // regular octahedron faces
    val RO_Top       = toRegularFace(O_Top)
    val RO_Bottom    = toRegularFace(O_Bottom)
    val RO_NorthEast = toRegularFace(O_NorthEast)
    val RO_North     = toRegularFace(O_North)
    val RO_NorthWest = toRegularFace(O_NorthWest)
    val RO_SouthWest = toRegularFace(O_SouthWest)
    val RO_South     = toRegularFace(O_South)
    val RO_SouthEast = toRegularFace(O_SouthEast)

    // regular downward tetrahedron faces
    val RDT_Top       = toRegularFace(DT_Top)
    val RDT_South     = toRegularFace(DT_South)
    val RDT_NorthWest = toRegularFace(DT_NorthWest)
    val RDT_NorthEast = toRegularFace(DT_NorthEast)

    // regular upward tetrahedron faces
    val RUT_Bottom    = toRegularFace(UT_Bottom)
    val RUT_North     = toRegularFace(UT_North)
    val RUT_SouthWest = toRegularFace(UT_SouthWest)
    val RUT_SouthEast = toRegularFace(UT_SouthEast)
  }
  import GyratedFaces._
  
  private case class Octahedron(i: Int, j: Int, k: Int) extends GTOHPolyhedron {
    override val sourcePolyhedron = Octahedron0
    override val faces = List(RO_Top, RO_Bottom, RO_NorthEast, RO_North, RO_NorthWest, RO_SouthWest, RO_South, RO_SouthEast)
  }
  
  private case class UpwardTetrahedron(i: Int, j: Int, k: Int) extends GTOHPolyhedron {
    override val sourcePolyhedron = UpwardTetrahedron0
    override val faces = List(RUT_Bottom, RUT_North, RUT_SouthWest, RUT_SouthEast)
  }
  
  private case class DownwardTetrahedron(i: Int, j: Int, k: Int) extends GTOHPolyhedron {
    override val sourcePolyhedron = DownwardTetrahedron0
    override val faces = List(RDT_Top, RDT_South, RDT_NorthWest, RDT_NorthEast)
  }
  
  private case class GyratedOctahedron(i: Int, j: Int, k: Int) extends GTOHPolyhedron {
    override val sourcePolyhedron = GyratedOctahedron0
  }
  
  private case class GyratedUpwardTetrahedron(i: Int, j: Int, k: Int) extends GTOHPolyhedron {
    override val sourcePolyhedron = GyratedUpwardTetrahedron0
  }
  
  private case class GyratedDownwardTetrahedron(i: Int, j: Int, k: Int) extends GTOHPolyhedron {
    override val sourcePolyhedron = GyratedDownwardTetrahedron0
  }
  
  private object GyratedOctahedron0 extends GyratedOctahedron(0 ,0, 0) {
    override val name = "GyratedOctahedron0"
    override val id = Octahedron0.id
    override val faces = List(GO_Top, GO_Bottom, GO_NorthEast, GO_North, GO_NorthWest, GO_SouthWest, GO_South, GO_SouthEast)
    override lazy val center = Octahedron0.center
  }
  
  private object GyratedUpwardTetrahedron0 extends GyratedUpwardTetrahedron(0 ,0, 0) {
    override val id = UpwardTetrahedron0.id
    override val name = "GyratedUpwardTetrahedron0"
    override val faces = List(GUT_Bottom, GUT_South, GUT_NorthWest, GUT_NorthEast)
    override lazy val center = ySymmetry(UpwardTetrahedron0.center, Octahedron0.center.y)
  }
  
  private object GyratedDownwardTetrahedron0 extends GyratedDownwardTetrahedron(0 ,0, 0) {
    override val id = DownwardTetrahedron0.id
    override val name = "GyratedDownwardTetrahedron0"
    override val faces = List(GDT_Top, GDT_North, GDT_SouthWest, GDT_SouthEast)
    override lazy val center = ySymmetry(DownwardTetrahedron0.center, Octahedron0.center.y)
  }
  
  private lazy val gyratedFaces = ((GyratedOctahedron0.faces) ++ (GyratedUpwardTetrahedron0.faces) ++ (GyratedDownwardTetrahedron0.faces)).map(e => (e.id, e)).toMap

}