package engine.rendering

import engine.{Scene}
import Renderer._
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.Display
import perf.Perf.perfed
import models.Point3f
import models.container.{Boundable, Octree}
import util.Collections.successivePairsCycling
import engine.Camera
import math._

abstract class Renderer(var width: Int, var height: Int) {
  def init: Unit
  def render(picking: Boolean = false, lastHit: Option[ID] = None): Option[ID]
}

object DefaultRenderer {
  
  import util.Math.SQRTS._
  
  val P1 = new Point3f (1f, -1f / SQRT_3, -1f / SQRT_6)
  val P2 = new Point3f (-1f, -1f / SQRT_3, -1f / SQRT_6)
  val P3 = new Point3f (0, 2 / SQRT_3, -1f / SQRT_6)
  val P4 = new Point3f (0, 0, 3f / SQRT_6)
  
  val I1 = new Point3f (-1f, 1f / SQRT_3, 1f / SQRT_6)
  val I2 = new Point3f (1f, 1f / SQRT_3, 1f / SQRT_6)
  val I3 = new Point3f (0, -2f / SQRT_3, 1f / SQRT_6)
  val I4 = new Point3f (0, 0, -3f / SQRT_6)
  
  val shaped = Seq(
        ((P1, P3, P4), -P2.normalize),
        ((P2, P1, P4), -P3.normalize),
        ((P1, P2, P3), -P4.normalize),
        ((P2, P4, P3), -P1.normalize),
        ((I3, I1, I4), -I2.normalize),
        ((I1, I2, I4), -I3.normalize),
        ((I2, I1, I3), -I4.normalize),
        ((I4, I2, I3), -I1.normalize)
        )
        
  def playerPositionToPlayerRenderables(at: Point3f): Seq[Renderable] =
    shaped map { case (triangle, n) => new Renderable {
      def color = (-105, 40, -75)
      
      def normal = n
    
      def toTriangles = Seq((at + triangle._1, at + triangle._2, at + triangle._3))
    
      def toContour = {
        val p1 = triangle._1
        val p2 = triangle._2
        val p3 = triangle._3
        Seq(p1, p2, p3, (p3+p1)/2, (p2+p3)/2, (p1+p2)/2, (p3+p1)/2) map ( _ + at )
      }
      
    } }
}

import DefaultRenderer._

class DefaultRenderer(camera: Camera, w: Int, h: Int) extends Renderer(w, h) {

  var visibles = Map.empty[ID, Pickable]
  var players = Map.empty[Int, (Point3f, Float, Float)]
  var sunAngle = 0d
    
  // artificial ground
  import scala.collection.mutable.ArrayBuffer
  val wireframes = perfed("grid floor") {
    import sandbox.Shapes
    Shapes.gridFloor(20)
  }
  
  val sun = new SpecialRenderable {
    val sunDistance = 100
    val sunSize = 10
    val winterFactor = 0.5f
    val summerFactor = math.sqrt(1 - winterFactor * winterFactor).toFloat

    def render {
      glPushMatrix

      val sunX = summerFactor*sunDistance*sin(sunAngle).toFloat
      val sunY = summerFactor*sunDistance*cos(sunAngle).toFloat
      glTranslatef(sunX,-sunDistance*winterFactor,sunY)
      glRotatef(sunAngle.toDegrees.toFloat,0,1,0)
      glRotatef(45,1,0,0)

      // sun face
      glColor3f(1f, 1f, 0.7f)
      glDisable(GL_LIGHTING)
      glDisable(GL_TEXTURE)
      glPolygonMode(GL_FRONT, GL_FILL)
      glBegin(GL_QUADS)
      glVertex3f(sunSize,sunSize,0)
      glVertex3f(sunSize,-sunSize,0)
      glVertex3f(-sunSize,-sunSize,0)
      glVertex3f(-sunSize,sunSize,0)
      glEnd

      // sun light
      glLightBuff(GL_LIGHT0, GL_POSITION, Array(0f,0,1,0))
      //    glLightBuff(GL_LIGHT0, GL_AMBIENT, Array(0f,0,0,0))
      //    glLightBuff(GL_LIGHT0, GL_SPECULAR, Array(0f,0,0,0))
      glLightBuff(GL_LIGHT0, GL_DIFFUSE, Array(0.8f,0.4f,0.2f,0))

      glPopMatrix
    }
  }

  val moon = new SpecialRenderable {
    val moonDistance = 100
    val moonSize = 10
    val angle = 1.5f
    val x = moonDistance*sin(angle).toFloat
    val y = moonDistance*cos(angle).toFloat
    
    def render {
      glPushMatrix

      glTranslatef(x,0,y)
      glRotatef(angle.toDegrees.toFloat,0,1,0)

      // moon face
      glColor3f(0.6f, 0.6f, 0.7f)
      glDisable(GL_LIGHTING)
      glDisable(GL_TEXTURE)
      glPolygonMode(GL_FRONT, GL_FILL)
      glBegin(GL_QUADS)
      glVertex3f(moonSize,moonSize,0)
      glVertex3f(moonSize,-moonSize,0)
      glVertex3f(-moonSize,-moonSize,0)
      glVertex3f(-moonSize,moonSize,0)
      glEnd

      glPopMatrix
    }
  }
  
  val translationlessRenderables = Set(moon, sun)

  def init() {

    println(s"OpenGL version: ${glGetString(GL_VERSION)} from ${glGetString(GL_VENDOR)}")
    println("init gl")

    glEnable(GL_DEPTH_TEST)
    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)
    glEnable(GL_COLOR_MATERIAL)
    glEnable(GL_CULL_FACE)
    glEnable(GL_NORMALIZE)
    //glEnable(GL_LINE_SMOOTH)
    glPolygonMode(GL_FRONT, GL_FILL)
    
    // no global ambient light
    glLightModelBuff(GL_LIGHT_MODEL_AMBIENT, Array(0f,0,0,0))
    
    //glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE)

    val nearest = 0.2f
    val v = nearest * Display.getDisplayMode.getWidth.toDouble / Display.getDisplayMode.getHeight.toDouble
    println("v:%f", v)

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity

    glFrustum(-v, v, -nearest, nearest, nearest, 110)
    glMatrixMode(GL_MODELVIEW)

    // sun light color
    //glLightBuff(GL_LIGHT0, GL_DIFFUSE, Array(1f,1f,0f,0))

    // 'moonlight' ambient
    glLightBuff(GL_LIGHT0, GL_AMBIENT, Array(0.2f,0.3f,0.4f,0))
  }
  
  def render(picking: Boolean = false, lastHit: Option[ID] = None): Option[ID] = perfed ("render") {
    if (picking) {
      glDisable(GL_LIGHTING)
      glDisable(GL_LIGHT0)
      glDisable(GL_TEXTURE)
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      glLoadIdentity

      glPushMatrix

      camera.applyToWorld
      
      glBegin(GL_TRIANGLES)
      val inRange = perfed("visible") { Picking.filter(visibles.values, camera.position) }
      inRange.foreach{ case (pickingColor, g) => render(g, pickingColor, g.normal) }
      glEnd

      glPopMatrix

      glEnable(GL_LIGHT0)
      glEnable(GL_LIGHTING)
      render(false, Picking.readPicking((width / 2, height / 2), inRange, {p: Pickable => p.id}))
      //lastHit
    } else {
      glClear(GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      glLoadIdentity

      glPushMatrix

      camera.rotateWorld
      renderInfinitelyFar
      camera.translateWorld

      renderObjects(lastHit)
      renderPlayers
      renderGrid

      glPopMatrix

      renderHud

      glFlush

      lastHit
    }
  }

  protected def render(r: Renderable) {
    render(r, r.color, r.normal)
  }
  
  protected def renderPicked(r: Pickable) {
    render(r, r.pickedColor, r.normal)
  }
  
  protected def renderLine(r: Renderable) {
    glColor3ub(r.color._1, r.color._2, r.color._3)
    successivePairsCycling(r.toContour.toList).map{ case (p1, p2) =>
      glVertex3f(p1.x, p1.y, p1.z)
      glVertex3f(p2.x, p2.y, p2.z)
    }
  }
  
  protected def crosshair {
    glColor3f(0.7f, 0.9f, 0f)
    glBegin(GL_TRIANGLES)
    glVertex3d(0, -0.01924, -1)
    glVertex3d(0.01666, 0.0096, -1)
    glVertex3d(-0.01666, 0.0096, -1)
    glEnd
  }

  protected def drawTriangle(t: (Point3f, Point3f, Point3f)) {
    glVertex3f(t._1.x, t._1.y, t._1.z)
    glVertex3f(t._2.x, t._2.y, t._2.z)
    glVertex3f(t._3.x, t._3.y, t._3.z)
  }
  
  private def render(r: Renderable, c: Color3B, n: Point3f) {
    glColor3ub(c._1, c._2, c._3)
    glNormal3f(n.x, n.y, n.z)
    r.toTriangles.map(drawTriangle)
  }
  
  private def renderPlayers {
    glEnable(GL_LIGHTING)
    glPolygonMode(GL_FRONT, GL_FILL)
    glEnable( GL_POLYGON_OFFSET_FILL )      
    glPolygonOffset( 1f, 1f )
    
    for ( player <- players.values ;
          pos = player._1 ) {
     
      glPushMatrix
    
      glTranslatef(pos._1, pos._2, pos._3)
      glRotatef(player._2.toDegrees.toFloat, 0, 0, -1)
      glTranslatef(-pos._1, -pos._2, -pos._3)
    
      glBegin(GL_TRIANGLES)
      playerPositionToPlayerRenderables(pos) map render
      glEnd
    
      glDisable(GL_LIGHTING)
      glDisable( GL_POLYGON_OFFSET_FILL )
      glPolygonMode(GL_FRONT, GL_LINE)
      glColor3f(0.2f,0.2f,0.2f)
    
      glBegin(GL_LINES)
      playerPositionToPlayerRenderables(pos) map renderLine
      glColor3ub(-1, 0, 0)
      glVertex3f(pos.x, pos.y, pos.z)
      glVertex3f(pos.x, pos.y + 2 * sin(player._3).toFloat, pos.z - 2 * cos(player._3).toFloat)
    
      glEnd
      
      glPopMatrix
    }
  }
    
  private def renderObjects(lastHit: Option[ID]) {
    glEnable(GL_LIGHTING)
    glPolygonMode(GL_FRONT, GL_FILL)
    glEnable( GL_POLYGON_OFFSET_FILL )      
    glPolygonOffset( 1f, 1f )
    
    glBegin(GL_TRIANGLES)
    lastHit match {
      case None => visibles.values map { render(_) }
      case Some(faceId) => visibles.values.map { g =>
        if (g.id == faceId)
          renderPicked(g)
        else
          render(g)
      }
    }
    glEnd
    
    glDisable(GL_LIGHTING)
    glDisable( GL_POLYGON_OFFSET_FILL )
    glPolygonMode(GL_FRONT, GL_LINE)
    glColor3f(1,1,1)
    glBegin(GL_LINES)
    visibles.values.map { renderLine(_) }
    glEnd
  }

  private def renderInfinitelyFar {
    translationlessRenderables.foreach(_.render)
  }

  private def renderHud() {
    glDisable(GL_LIGHTING)
    glDisable(GL_DEPTH_TEST)
    glPolygonMode(GL_FRONT, GL_LINE)
    // triangle lime green crosshair
    crosshair
    glEnable(GL_LIGHTING)
    glEnable(GL_DEPTH_TEST)
    glPolygonMode(GL_FRONT, GL_FILL)
  }

  private def renderGrid() {
    // this creates the nice looking background.
    glDisable(GL_LIGHTING)
    glLineWidth(1)
    glBegin(GL_LINES)
    wireframes.map(renderLine)
    glEnd
  }
}

object Renderer {

  def glLightBuff(glLightEnum: Int, glConstant: Int, values: Array[Float]) {
    val buffer = BufferUtils.createFloatBuffer(values.length)
    buffer.put(values)
    buffer.flip
    glLight(glLightEnum, glConstant, buffer)
  }
  
  def glLightModelBuff(glLightModelEnum: Int, values: Array[Float]) {
    val buffer = BufferUtils.createFloatBuffer(values.length)
    buffer.put(values)
    buffer.flip
    glLightModel(glLightModelEnum, buffer)
  }
}