package models
import Honeycomb._

class TriakisTruncatedTetraHoneycomb extends Honeycomb {
  import TriakisTruncatedTetraHoneycomb._
  import util.Math.%+
  def polyhedron(i: Int, j: Int, k: Int, polyId: Int) = if ( %+(polyId,2) == 0 ) DownwardTriakisTruncatedTetra(i,j,k) else UpwardTriakisTruncatedTetra(i,j,k)
  def polyhedrons(i: Int, j: Int, k: Int) = List(DownwardTriakisTruncatedTetra(i,j,k), UpwardTriakisTruncatedTetra(i,j,k))
}

object TriakisTruncatedTetraHoneycomb {
  
  object Points {
    import util.Math.SQRTS._
    val TetraBottom  = new Point3f(0, 0, -2*SQRT_6_BY_3/3)*1.5f
    val TetraSouth   = new Point3f(0, -SQRT_3_BY_3, SQRT_6_BY_3/3)*1.5f
    val TetraNW      = new Point3f(-0.5, SQRT_3_BY_6, SQRT_6_BY_3/3)*1.5f
    val TetraNE      = new Point3f(0.5, SQRT_3_BY_6, SQRT_6_BY_3/3)*1.5f

    private def third(from: Point3f, to: Point3f) = from + (to - from) / 3

    private def barycenter(points: Iterable[Point3f], count: Int) =
      points.foldLeft(new Point3f(0,0,0))((z ,e) => z + e) / count

    // points after truncation of vertice
    val BottomS    = third(TetraBottom, TetraSouth)
    val BottomNW   = third(TetraBottom, TetraNW)
    val BottomNE   = third(TetraBottom, TetraNE)
    val Bottom     = barycenter(List(BottomS, BottomNW, BottomNE, TetraBottom), 4)
    val SouthB     = third(TetraSouth,   TetraBottom)
    val SouthNW    = third(TetraSouth,   TetraNW)
    val SouthNE    = third(TetraSouth,   TetraNE)
    val South      = barycenter(List(SouthB, SouthNW, SouthNE, TetraSouth), 4)
    val NorthWestB = third(TetraNW,  TetraBottom)
    val NorthWestS = third(TetraNW,  TetraSouth)
    val NorthWestNE= third(TetraNW,  TetraNE)
    val NorthWest  = barycenter(List(NorthWestB, NorthWestS, NorthWestNE, TetraNW), 4)
    val NorthEastB = third(TetraNE,  TetraBottom)
    val NorthEastS = third(TetraNE,  TetraSouth)
    val NorthEastNW= third(TetraNE,  TetraNW)
    val NorthEast  = barycenter(List(NorthEastB, NorthEastS, NorthEastNW, TetraNE), 4)
  }
  import Points._

  object Faces {

    trait TriakisFace {
      def id: Int
      def opposite(i: Int, j: Int, k: Int) =
        id match {
          case HexaT.id  => new FaceId(i,  j,  k+1, HexaB)
          case HexaSE.id => new FaceId(i+1,j-1,k,   HexaNW)
          case HexaSW.id => new FaceId(i,  j-1,k,   HexaNE)
          case HexaN.id  => new FaceId(i,  j,  k,   HexaS)
          case B0.id     => new FaceId(i+1,j-1,k-1, NW1)
          case B1.id     => new FaceId(i,  j,  k-1, S1)
          case B2.id     => new FaceId(i,  j-1,k-1, NE1)
          case S0.id     => new FaceId(i+1,j-1,k,   NW2)
          case S1.id     => new FaceId(i,  j,  k+1, B1)
          case S2.id     => new FaceId(i,  j-1,k,   NE2)
          case NW0.id    => new FaceId(i-1,j,  k,   NE0)
          case NW1.id    => new FaceId(i-1,j+1,k+1, B0)
          case NW2.id    => new FaceId(i-1,j+1,k,   S0)
          case NE0.id    => new FaceId(i+1,j,  k,   NW0)
          case NE1.id    => new FaceId(i,  j+1,k+1, B2)
          case NE2.id    => new FaceId(i,  j+1,k,   S2)
          case HexaB.id  => new FaceId(i,  j,  k-1, HexaT)
          case HexaNW.id => new FaceId(i-1,j+1,k,   HexaSE)
          case HexaNE.id => new FaceId(i,  j+1,k,   HexaSW)
          case HexaS.id  => new FaceId(i,  j,  k,   HexaN)
          case T0.id     => new FaceId(i-1,j+1,k+1, SE1)
          case T1.id     => new FaceId(i,  j,  k+1, N1)
          case T2.id     => new FaceId(i,  j+1,k+1, SW1)
          case N0.id     => new FaceId(i-1,j+1,k,   SE2)
          case N1.id     => new FaceId(i,  j,  k-1, T1)
          case N2.id     => new FaceId(i,  j+1,k,   SW2)
          case SE0.id    => new FaceId(i+1,j,  k,   SW0)
          case SE1.id    => new FaceId(i+1,j-1,k-1, T0)
          case SE2.id    => new FaceId(i+1,j-1,k,   N0)
          case SW0.id    => new FaceId(i-1,j,  k,   SE0)
          case SW1.id    => new FaceId(i,  j-1,k-1, T2)
          case SW2.id    => new FaceId(i,  j-1,k,   N2)
      }
    }

    val firstId = TriakisTruncatedTetra1.id
    object B0  extends Face(firstId, new Triangle(Bottom, BottomNE, BottomS)) with TriakisFace
    object B1  extends Face(firstId, new Triangle(Bottom, BottomNW, BottomNE)) with TriakisFace
    object B2  extends Face(firstId, new Triangle(Bottom, BottomS, BottomNW)) with TriakisFace
    object S0  extends Face(firstId, new Triangle(South, SouthB, SouthNE)) with TriakisFace
    object S1  extends Face(firstId, new Triangle(South, SouthNE, SouthNW)) with TriakisFace
    object S2  extends Face(firstId, new Triangle(South, SouthNW, SouthB)) with TriakisFace
    object NW0 extends Face(firstId, new Triangle(NorthWest, NorthWestB, NorthWestS)) with TriakisFace
    object NW1 extends Face(firstId, new Triangle(NorthWest, NorthWestS, NorthWestNE)) with TriakisFace
    object NW2 extends Face(firstId, new Triangle(NorthWest, NorthWestNE, NorthWestB)) with TriakisFace
    object NE0 extends Face(firstId, new Triangle(NorthEast, NorthEastS, NorthEastB)) with TriakisFace
    object NE1 extends Face(firstId, new Triangle(NorthEast, NorthEastNW, NorthEastS)) with TriakisFace
    object NE2 extends Face(firstId, new Triangle(NorthEast, NorthEastB, NorthEastNW)) with TriakisFace

    object HexaT  extends Face(firstId, new Hexad(List(NorthEastS, NorthEastNW, NorthWestNE, NorthWestS, SouthNW, SouthNE))) with TriakisFace
    object HexaSE extends Face(firstId, new Hexad(List(NorthEastS, NorthEastB, BottomNE, BottomS, SouthB, SouthNE).reverse)) with TriakisFace
    object HexaSW extends Face(firstId, new Hexad(List(NorthWestS, NorthWestB, BottomNW, BottomS, SouthB, SouthNW))) with TriakisFace
    object HexaN  extends Face(firstId, new Hexad(List(NorthEastNW, NorthEastB, BottomNE, BottomNW, NorthWestB, NorthWestNE))) with TriakisFace

    val Tetra1Faces: List[Face] = List(
      HexaT, HexaSE, HexaSW, HexaN,
      B0, B1, B2,
      S0, S1, S2,
      NW0, NW1, NW2,
      NE0, NE1, NE2)

    val northTranslation = NorthEastNW + BottomNW
    val southEastTranslation = SouthNE + BottomNE
    val southWestTranslation = NorthWestS + BottomS
    val upTranslation = new Point3f(0,0,NorthWestS.z - BottomNW.z)

    private val TetraFaces: Map[Face, Face] = Tetra1Faces.map(f => (f, new Face(TriakisTruncatedTetra2.id, f.polygon.pointSymetry(TriakisTruncatedTetra1.center).reverse.translate(northTranslation)) with TriakisFace)).toMap

    val HexaB  = TetraFaces(HexaT)
    val HexaNW = TetraFaces(HexaSE)
    val HexaNE = TetraFaces(HexaSW)
    val HexaS  = TetraFaces(HexaN)
    val T0     = TetraFaces(B0)
    val T1     = TetraFaces(B1)
    val T2     = TetraFaces(B2)
    val N0     = TetraFaces(S0)
    val N1     = TetraFaces(S1)
    val N2     = TetraFaces(S2)
    val SE0    = TetraFaces(NW0)
    val SE1    = TetraFaces(NW1)
    val SE2    = TetraFaces(NW2)
    val SW0    = TetraFaces(NE0)
    val SW1    = TetraFaces(NE1)
    val SW2    = TetraFaces(NE2)

    val Tetra2Faces = List(
      HexaB, HexaNE, HexaNW, HexaS,
      T0, T1, T2,
      N0, N1, N2,
      SE0, SE1, SE2,
      SW0, SW1, SW2)
  }
  import Faces._

  sealed trait TriakisTruncatedTetra extends Polyhedron {
    lazy val origin =
      new Point3f(i * 2*southEastTranslation.x + j * southEastTranslation.x,
        j * (northTranslation.y - southEastTranslation.y) - k * northTranslation.y,
        k * upTranslation.z)
  }

  case class DownwardTriakisTruncatedTetra(i: Int, j: Int, k: Int) extends TriakisTruncatedTetra {
    val sourcePolyhedron = TriakisTruncatedTetra1
  }

  case class UpwardTriakisTruncatedTetra(i: Int, j: Int, k: Int) extends TriakisTruncatedTetra {
    val sourcePolyhedron = TriakisTruncatedTetra2
  }
  
  object TriakisTruncatedTetra1 extends DownwardTriakisTruncatedTetra(0,0,0) {
    override val name = "TriakisTruncatedTetra1"

    override def faces = Tetra1Faces

    override lazy val center = new Point3f(0,0,0)

    override val id = 0
  }

  object TriakisTruncatedTetra2 extends UpwardTriakisTruncatedTetra(0,0,0) {
    override val name = "TriakisTruncatedTetra2"

    override def faces = Tetra2Faces

    override lazy val center = northTranslation

    override val id = 1
  }
  
}