package models

import scala.collection.Iterable
import Honeycomb._

class BitruncatedCubicHoneycomb extends Honeycomb {

  import BitruncatedCubicHoneycomb.TruncatedOctahedron
  
  def polyhedron(i: Int, j: Int, k: Int, polyId: Int): Polyhedron = TruncatedOctahedron(i,j,k)
  def polyhedrons(i: Int, j: Int, k: Int): Iterable[Polyhedron] = Iterable(TruncatedOctahedron(i,j,k))
}

object BitruncatedCubicHoneycomb {
  import util.Math.%+

  private[models] object Points {
    object TopSquareN    extends Point3f ( 0, 0.5, 1)
    object TopSquareW    extends Point3f ( -0.5, 0, 1)
    object TopSquareS    extends Point3f ( 0, -0.5, 1)
    object TopSquareE    extends Point3f ( 0.5, 0, 1)
    object BottomSquareE extends Point3f ( 0.5, 0, -1)
    object BottomSquareS extends Point3f ( 0, -0.5, -1)
    object BottomSquareW extends Point3f ( -0.5, 0, -1)
    object BottomSquareN extends Point3f ( 0, 0.5, -1)
    object SouthSquareU  extends Point3f ( 0, -1, 0.5)
    object SouthSquareW  extends Point3f ( -0.5, -1, 0)
    object SouthSquareD  extends Point3f ( 0, -1, -0.5)
    object SouthSquareE  extends Point3f ( 0.5, -1, 0)
    object NorthSquareU  extends Point3f ( 0, 1, 0.5)
    object NorthSquareE  extends Point3f ( 0.5, 1, 0)
    object NorthSquareD  extends Point3f ( 0, 1, -0.5)
    object NorthSquareW  extends Point3f ( -0.5, 1, 0)
    object EastSquareD   extends Point3f ( 1, 0, -0.5)
    object EastSquareN   extends Point3f ( 1, 0.5, 0)
    object EastSquareU   extends Point3f ( 1, 0, 0.5)
    object EastSquareS   extends Point3f ( 1, -0.5, 0)
    object WestSquareU   extends Point3f ( -1, 0, 0.5)
    object WestSquareN   extends Point3f ( -1, 0.5, 0)
    object WestSquareD   extends Point3f ( -1, 0, -0.5)
    object WestSquareS   extends Point3f ( -1, -0.5, 0)
  }
  import Points._
  
  private[models] object Faces {

    trait BTHFace {
      def id: Int
      def opposite(i: Int, j: Int, k: Int) = {
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

    object TopSquare    extends Face(0, 0,
                                new Quad(List(TopSquareN,
                                         TopSquareW,
                                         TopSquareS,
                                         TopSquareE))) with BTHFace
    object BottomSquare extends Face(1, 0,
                                new Quad(List(BottomSquareE,
                                         BottomSquareS,
                                         BottomSquareW,
                                         BottomSquareN))) with BTHFace
    object NorthSquare  extends Face(2, 0,
                                new Quad(List(NorthSquareU,
                                         NorthSquareE,
                                         NorthSquareD,
                                         NorthSquareW))) with BTHFace
    object SouthSquare  extends Face(3, 0,
                                new Quad(List(SouthSquareU,
                                         SouthSquareW,
                                         SouthSquareD,
                                         SouthSquareE))) with BTHFace
    object EastSquare   extends Face(4, 0,
                                new Quad(List(EastSquareD,
                                         EastSquareN,
                                         EastSquareU,
                                         EastSquareS))) with BTHFace
    object WestSquare   extends Face(5, 0,
                                new Quad(List(WestSquareU,
                                         WestSquareN,
                                         WestSquareD,
                                         WestSquareS))) with BTHFace
    object TopNorthEastHexa extends Face(6, 0,
                                    new Hexad(List(TopSquareN,
                                        TopSquareE,
                                        EastSquareU,
                                        EastSquareN,
                                        NorthSquareE,
                                        NorthSquareU))) with BTHFace
    object TopNorthWestHexa extends Face(7, 0,
                                    new Hexad(List(TopSquareW,
                                        TopSquareN,
                                        NorthSquareU,
                                        NorthSquareW,
                                        WestSquareN,
                                        WestSquareU))) with BTHFace
    object TopSouthWestHexa extends Face(8, 0,
                                    new Hexad(List(TopSquareS,
                                        TopSquareW,
                                        WestSquareU,
                                        WestSquareS,
                                        SouthSquareW,
                                        SouthSquareU))) with BTHFace
    object TopSouthEastHexa extends Face(9, 0,
                                    new Hexad(List(TopSquareE,
                                        TopSquareS,
                                        SouthSquareU,
                                        SouthSquareE,
                                        EastSquareS,
                                        EastSquareU))) with BTHFace
    object BottomNorthEastHexa extends Face(10, 0,
                                    new Hexad(List(BottomSquareN,
                                        BottomSquareE,
                                        EastSquareD,
                                        EastSquareN,
                                        NorthSquareE,
                                        NorthSquareD).reverse)) with BTHFace
    object BottomNorthWestHexa extends Face(11, 0,
                                    new Hexad(List(BottomSquareW,
                                        BottomSquareN,
                                        NorthSquareD,
                                        NorthSquareW,
                                        WestSquareN,
                                        WestSquareD).reverse)) with BTHFace
    object BottomSouthWestHexa extends Face(12, 0,
                                    new Hexad(List(BottomSquareS,
                                        BottomSquareW,
                                        WestSquareD,
                                        WestSquareS,
                                        SouthSquareW,
                                        SouthSquareD).reverse)) with BTHFace
    object BottomSouthEastHexa extends Face(13, 0,
                                    new Hexad(List(BottomSquareE,
                                        BottomSquareS,
                                        SouthSquareD,
                                        SouthSquareE,
                                        EastSquareS,
                                        EastSquareD).reverse)) with BTHFace
  }
  import Faces._
  
  case class TruncatedOctahedron(i: Int, j: Int, k: Int) extends Polyhedron {
    val sourcePolyhedron = TruncatedOctahedron0
    
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
  
//  object TOFaceCameraFilter {
//
//    
//    def byPitchAndYaw(yaw: Double, pitch: Double) = byYaw(yaw).intersect(byPitch(pitch))
//    
//    private def byYaw(yaw: Double) = {
//      if ( yaw == 0)
//        HashSet(TopSquare.id,
//          TopNorthEastHexa.id,
//          TopNorthWestHexa.id,
//          TopSouthWestHexa.id,
//          TopSouthEastHexa.id)
//      else if ( yaw <= math.Pi / 4 )
//        HashSet(TopSquare.id,
//          NorthSquare.id,
//          SouthSquare.id,
//          EastSquare.id,
//          WestSquare.id,
//          TopNorthEastHexa.id,
//          TopNorthWestHexa.id,
//          TopSouthWestHexa.id,
//          TopSouthEastHexa.id)
//      else if ( yaw < math.Pi / 2 )
//        HashSet(TopSquare.id,
//          NorthSquare.id,
//          SouthSquare.id,
//          EastSquare.id,
//          WestSquare.id,
//          TopNorthEastHexa.id,
//          TopNorthWestHexa.id,
//          TopSouthWestHexa.id,
//          TopSouthEastHexa.id,
//          BottomNorthEastHexa.id,
//          BottomNorthWestHexa.id,
//          BottomSouthWestHexa.id,
//          BottomSouthEastHexa.id)
//      else if ( yaw == math.Pi / 2 )
//        HashSet(NorthSquare.id,
//          SouthSquare.id,
//          EastSquare.id,
//          WestSquare.id,
//          TopNorthEastHexa.id,
//          TopNorthWestHexa.id,
//          TopSouthWestHexa.id,
//          TopSouthEastHexa.id,
//          BottomNorthEastHexa.id,
//          BottomNorthWestHexa.id,
//          BottomSouthWestHexa.id,
//          BottomSouthEastHexa.id)
//      else if ( yaw < 3 * math.Pi / 4 )
//        HashSet(BottomSquare.id,
//          NorthSquare.id,
//          SouthSquare.id,
//          EastSquare.id,
//          WestSquare.id,
//          TopNorthEastHexa.id,
//          TopNorthWestHexa.id,
//          TopSouthWestHexa.id,
//          TopSouthEastHexa.id,
//          BottomNorthEastHexa.id,
//          BottomNorthWestHexa.id,
//          BottomSouthWestHexa.id,
//          BottomSouthEastHexa.id)
//      else if ( yaw < math.Pi )
//        HashSet(BottomSquare.id,
//          NorthSquare.id,
//          SouthSquare.id,
//          EastSquare.id,
//          WestSquare.id,
//          BottomNorthEastHexa.id,
//          BottomNorthWestHexa.id,
//          BottomSouthWestHexa.id,
//          BottomSouthEastHexa.id)
//     else // yaw == math.Pi
//       HashSet(BottomSquare.id,
//          BottomNorthEastHexa.id,
//          BottomNorthWestHexa.id,
//          BottomSouthWestHexa.id,
//          BottomSouthEastHexa.id)
//    }
//    
//    private def byPitch(pitch: Double) = HashSet(TopSquare.id, BottomSquare.id) ++ {
//      if ( pitch == 0)
//        HashSet(SouthSquare.id,
//          BottomSouthWestHexa.id,
//          BottomSouthEastHexa.id,
//          TopSouthWestHexa.id,
//          TopSouthEastHexa.id)
//      else if ( pitch < math.Pi / 4 )
//        HashSet(SouthSquare.id,
//          BottomSouthWestHexa.id,
//          BottomSouthEastHexa.id,
//          TopSouthWestHexa.id,
//          TopSouthEastHexa.id,
//          WestSquare.id)
//      else if ( pitch == math.Pi / 4 )
//        HashSet(SouthSquare.id,
//          BottomSouthWestHexa.id,
//          TopSouthWestHexa.id,
//          WestSquare.id)
//      else if ( pitch < math.Pi / 2 )
//        HashSet(SouthSquare.id,
//          BottomSouthWestHexa.id,
//          BottomNorthWestHexa.id,
//          TopSouthWestHexa.id,
//          TopNorthWestHexa.id,
//          WestSquare.id)
//      else if ( pitch == math.Pi / 2 )
//        HashSet(BottomSouthWestHexa.id,
//          BottomNorthWestHexa.id,
//          TopSouthWestHexa.id,
//          TopNorthWestHexa.id,
//          WestSquare.id)
//      else if ( pitch < 3 * math.Pi / 4 )
//        HashSet(BottomSouthWestHexa.id,
//          BottomNorthWestHexa.id,
//          TopSouthWestHexa.id,
//          TopNorthWestHexa.id,
//          WestSquare.id,
//          NorthSquare.id)
//      else if ( pitch == 3 * math.Pi / 4 )
//        HashSet(BottomNorthWestHexa.id,
//          TopNorthWestHexa.id,
//          WestSquare.id,
//          NorthSquare.id)
//      else if ( pitch < math.Pi )
//        HashSet(BottomNorthWestHexa.id,
//          BottomNorthEastHexa.id,
//          TopNorthWestHexa.id,
//          TopNorthEastHexa.id,
//          WestSquare.id,
//          NorthSquare.id)
//      else if ( pitch == math.Pi )
//        HashSet(BottomNorthWestHexa.id,
//          BottomNorthEastHexa.id,
//          TopNorthWestHexa.id,
//          TopNorthEastHexa.id,
//          NorthSquare.id)
//      else if ( pitch < 5 * math.Pi / 4 )
//        HashSet(BottomNorthWestHexa.id,
//          BottomNorthEastHexa.id,
//          TopNorthWestHexa.id,
//          TopNorthEastHexa.id,
//          NorthSquare.id,
//          EastSquare.id)
//      else if ( pitch == 5 * math.Pi / 4 )
//        HashSet(BottomNorthEastHexa.id,
//          TopNorthEastHexa.id,
//          NorthSquare.id,
//          EastSquare.id)
//      else if ( pitch < 3 * math.Pi / 2 )
//        HashSet(BottomNorthEastHexa.id,
//          BottomSouthEastHexa.id,
//          TopNorthEastHexa.id,
//          TopSouthEastHexa.id,
//          NorthSquare.id,
//          EastSquare.id)
//      else if ( pitch == 3 * math.Pi / 2 )
//        HashSet(BottomNorthEastHexa.id,
//          BottomSouthEastHexa.id,
//          TopNorthEastHexa.id,
//          TopSouthEastHexa.id,
//          EastSquare.id)
//      else if ( pitch < 7 * math.Pi / 4 )
//        HashSet(BottomNorthEastHexa.id,
//          BottomSouthEastHexa.id,
//          TopNorthEastHexa.id,
//          TopSouthEastHexa.id,
//          EastSquare.id,
//          SouthSquare.id)
//      else if ( pitch == 7 * math.Pi / 4 )
//        HashSet(BottomSouthEastHexa.id,
//          TopNorthEastHexa.id,
//          EastSquare.id,
//          SouthSquare.id)
//      else // ( pitch < 2 * math.Pi )
//        HashSet(BottomSouthEastHexa.id,
//          BottomSouthWestHexa.id,
//          TopSouthEastHexa.id,
//          TopSouthWestHexa.id,
//          EastSquare.id,
//          SouthSquare.id)
//    }
//  }
}