package models

import scala.collection.Iterable
import Honeycomb._

class BitruncatedCubicHoneycomb extends Honeycomb {

  import BitruncatedCubicHoneycomb.TruncatedOctahedron
  import perf.Perf.perfed
  
  def polyhedron(i: Int, j: Int, k: Int, polyId: Int): Polyhedron = perfed("poly") {new TruncatedOctahedron(i,j,k)}
  def polyhedrons(i: Int, j: Int, k: Int): Iterable[Polyhedron] = perfed("polys") {Iterable(new TruncatedOctahedron(i,j,k))}
}

object BitruncatedCubicHoneycomb {
  
  private[models] object Points {
    case object TopSquareN    extends Point3f ( 0, 0.5, 1)
    case object TopSquareW    extends Point3f ( -0.5, 0, 1)
    case object TopSquareS    extends Point3f ( 0, -0.5, 1)
    case object TopSquareE    extends Point3f ( 0.5, 0, 1)
    case object BottomSquareE extends Point3f ( 0.5, 0, -1)
    case object BottomSquareS extends Point3f ( 0, -0.5, -1)
    case object BottomSquareW extends Point3f ( -0.5, 0, -1)
    case object BottomSquareN extends Point3f ( 0, 0.5, -1)
    case object SouthSquareU  extends Point3f ( 0, -1, 0.5)
    case object SouthSquareW  extends Point3f ( -0.5, -1, 0)
    case object SouthSquareD  extends Point3f ( 0, -1, -0.5)
    case object SouthSquareE  extends Point3f ( 0.5, -1, 0)
    case object NorthSquareU  extends Point3f ( 0, 1, 0.5)
    case object NorthSquareE  extends Point3f ( 0.5, 1, 0)
    case object NorthSquareD  extends Point3f ( 0, 1, -0.5)
    case object NorthSquareW  extends Point3f ( -0.5, 1, 0)
    case object EastSquareD   extends Point3f ( 1, 0, -0.5)
    case object EastSquareN   extends Point3f ( 1, 0.5, 0)
    case object EastSquareU   extends Point3f ( 1, 0, 0.5)
    case object EastSquareS   extends Point3f ( 1, -0.5, 0)
    case object WestSquareU   extends Point3f ( -1, 0, 0.5)
    case object WestSquareN   extends Point3f ( -1, 0.5, 0)
    case object WestSquareD   extends Point3f ( -1, 0, -0.5)
    case object WestSquareS   extends Point3f ( -1, -0.5, 0)
  }
  import Points._
  
  private[models] object Faces {

    trait BTHFace {
      def id: Int
      def opposite(i: Int, j: Int, k: Int) = {
        import engine.Math.%+
        val s = %+(k,2) // on odd k levels, neighbors are shifted by one on the xy plan
        id match {
          case TopSquare.id           => FaceId(i,   j,   k+2, BottomSquare)
          case BottomSquare.id        => FaceId(i,   j,   k-2, TopSquare)
          case NorthSquare.id         => FaceId(i,   j+1, k,   SouthSquare)
          case SouthSquare.id         => FaceId(i,   j-1, k,   NorthSquare)
          case EastSquare.id          => FaceId(i+1, j,   k,   WestSquare)
          case WestSquare.id          => FaceId(i-1, j,   k,   EastSquare)
          case TopNorthEastHexa.id    => FaceId(i+s,   j+s,   k+1, BottomSouthWestHexa)
          case TopNorthWestHexa.id    => FaceId(i-1+s, j+s,   k+1, BottomSouthEastHexa)
          case TopSouthWestHexa.id    => FaceId(i-1+s, j-1+s, k+1, BottomNorthEastHexa)
          case TopSouthEastHexa.id    => FaceId(i+s,   j-1+s, k+1, BottomNorthWestHexa)
          case BottomNorthEastHexa.id => FaceId(i+s,   j+s,   k-1, TopSouthWestHexa)
          case BottomNorthWestHexa.id => FaceId(i-1+s, j+s,   k-1, TopSouthEastHexa)
          case BottomSouthWestHexa.id => FaceId(i-1+s, j-1+s, k-1, TopNorthEastHexa)
          case BottomSouthEastHexa.id => FaceId(i+s,   j-1+s, k-1, TopNorthWestHexa)
        }
      }
    }

    object TopSquare    extends Face(0,
                                new Quad(List(TopSquareN,
                                         TopSquareW,
                                         TopSquareS,
                                         TopSquareE))) with BTHFace
    object BottomSquare extends Face(0,
                                new Quad(List(BottomSquareE,
                                         BottomSquareS,
                                         BottomSquareW,
                                         BottomSquareN))) with BTHFace
    object NorthSquare  extends Face(0,
                                new Quad(List(NorthSquareU,
                                         NorthSquareE,
                                         NorthSquareD,
                                         NorthSquareW))) with BTHFace
    object SouthSquare  extends Face(0,
                                new Quad(List(SouthSquareU,
                                         SouthSquareW,
                                         SouthSquareD,
                                         SouthSquareE))) with BTHFace
    object EastSquare   extends Face(0,
                                new Quad(List(EastSquareD,
                                         EastSquareN,
                                         EastSquareU,
                                         EastSquareS))) with BTHFace
    object WestSquare   extends Face(0,
                                new Quad(List(WestSquareU,
                                         WestSquareN,
                                         WestSquareD,
                                         WestSquareS))) with BTHFace
    object TopNorthEastHexa extends Face(0,
                                    new Hexad(List(TopSquareN,
                                        TopSquareE,
                                        EastSquareU,
                                        EastSquareN,
                                        NorthSquareE,
                                        NorthSquareU))) with BTHFace
    object TopNorthWestHexa extends Face(0,
                                    new Hexad(List(TopSquareW,
                                        TopSquareN,
                                        NorthSquareU,
                                        NorthSquareW,
                                        WestSquareN,
                                        WestSquareU))) with BTHFace
    object TopSouthWestHexa extends Face(0,
                                    new Hexad(List(TopSquareS,
                                        TopSquareW,
                                        WestSquareU,
                                        WestSquareS,
                                        SouthSquareW,
                                        SouthSquareU))) with BTHFace
    object TopSouthEastHexa extends Face(0,
                                    new Hexad(List(TopSquareE,
                                        TopSquareS,
                                        SouthSquareU,
                                        SouthSquareE,
                                        EastSquareS,
                                        EastSquareU))) with BTHFace
    object BottomNorthEastHexa extends Face(0,
                                    new Hexad(List(BottomSquareN,
                                        BottomSquareE,
                                        EastSquareD,
                                        EastSquareN,
                                        NorthSquareE,
                                        NorthSquareD).reverse)) with BTHFace
    object BottomNorthWestHexa extends Face(0,
                                    new Hexad(List(BottomSquareW,
                                        BottomSquareN,
                                        NorthSquareD,
                                        NorthSquareW,
                                        WestSquareN,
                                        WestSquareD).reverse)) with BTHFace
    object BottomSouthWestHexa extends Face(0,
                                    new Hexad(List(BottomSquareS,
                                        BottomSquareW,
                                        WestSquareD,
                                        WestSquareS,
                                        SouthSquareW,
                                        SouthSquareD).reverse)) with BTHFace
    object BottomSouthEastHexa extends Face(0,
                                    new Hexad(List(BottomSquareE,
                                        BottomSquareS,
                                        SouthSquareD,
                                        SouthSquareE,
                                        EastSquareS,
                                        EastSquareD).reverse)) with BTHFace
  }
  import Faces._
  
  class TruncatedOctahedron(val i: Int, val j: Int, val k: Int) extends Polyhedron {
    val sourcePolyhedron = TruncatedOctahedron0
    
    import engine.Math.%+
    def origin = new Point3f(i*2 + %+(k,2), j*2 + %+(k,2), k)
  }
  
  object TruncatedOctahedron0 extends TruncatedOctahedron(0,0,0) {
    override val id = 0
    override val name = "TruncatedOctahedron0"
    override val faces = List(TopSquare,
        BottomSquare,
        NorthSquare,
        SouthSquare,
        EastSquare,
        WestSquare,
        TopNorthEastHexa,
        TopNorthWestHexa,
        TopSouthWestHexa,
        TopSouthEastHexa,
        BottomNorthEastHexa,
        BottomNorthWestHexa,
        BottomSouthWestHexa,
        BottomSouthEastHexa)
    override lazy val center = new Point3f(0,0,0)
  }
}