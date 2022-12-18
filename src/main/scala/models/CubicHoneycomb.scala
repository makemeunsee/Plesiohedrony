package models
import Honeycomb._

class CubicHoneycomb extends Honeycomb {
  import CubicHoneycomb.Cube
  
  def polyhedron(i: Int, j: Int, k: Int, polyId: Int): Polyhedron = Cube(i,j,k)
  def polyhedrons(i: Int, j: Int, k: Int): Iterable[Polyhedron] = Iterable(Cube(i,j,k))
}

object CubicHoneycomb {
  
  object Points {
    object TopNE extends Point3f(0.5,0.5,0.5)
    object TopNW extends Point3f(-0.5,0.5,0.5)
    object TopSW extends Point3f(-0.5,-0.5,0.5)
    object TopSE extends Point3f(0.5,-0.5,0.5)
    object BottomNE extends Point3f(0.5,0.5,-0.5)
    object BottomNW extends Point3f(-0.5,0.5,-0.5)
    object BottomSW extends Point3f(-0.5,-0.5,-0.5)
    object BottomSE extends Point3f(0.5,-0.5,-0.5)
  }
  import Points._

  object Faces {
    trait CubicFace {
      def id: Int
      def opposite(i: Int, j: Int, k: Int) =
        id match {
          case Top.id    => FaceId(i, j, k+1, Bottom)
          case Bottom.id => FaceId(i, j, k-1, Top)
          case North.id  => FaceId(i, j+1, k, South)
          case South.id  => FaceId(i, j-1, k, North)
          case East.id   => FaceId(i+1, j, k, West)
          case West.id   => FaceId(i-1, j, k, East)
        }
    }

    object Top    extends Face(0, new Quad(List(TopNE,    TopNW,    TopSW,    TopSE))) with CubicFace
    object Bottom extends Face(0, new Quad(List(BottomSE, BottomSW, BottomNW, BottomNE))) with CubicFace
    object North  extends Face(0, new Quad(List(TopNW,    TopNE,    BottomNE, BottomNW))) with CubicFace
    object South  extends Face(0, new Quad(List(TopSE,    TopSW,    BottomSW, BottomSE))) with CubicFace
    object East   extends Face(0, new Quad(List(TopNE,    TopSE,    BottomSE, BottomNE))) with CubicFace
    object West   extends Face(0, new Quad(List(TopNW,    BottomNW, BottomSW, TopSW))) with CubicFace
  }
  import Faces._
  
  case class Cube(val i: Int, val j: Int, val k: Int) extends Polyhedron {
    val sourcePolyhedron = Cube0
    def origin = new Point3f(i, j, k)
  }
  
  object Cube0 extends Cube(0,0,0) {
    override lazy val center = new Point3f(0,0,0)
    override val id = 0
    override val name = "Cube0"
    override val faces = List(Top, Bottom, North, South, East, West)
  } 
}