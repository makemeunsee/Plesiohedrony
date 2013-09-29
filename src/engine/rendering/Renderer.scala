package engine.rendering

import engine.{Scene}
import Picking.Color3B
import Renderer._
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.Display

abstract class Renderer(val scene: Scene, var width: Int, var height: Int) {
  def init: Unit
  def render(picking: Boolean = false, lastHit: Option[ID] = None): Option[ID]
}

class DefaultRenderer(scene: Scene, width: Int, height: Int) extends Renderer(scene, width, height) {

  protected def visibles = scene.visible

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
    glLightBuff(GL_LIGHT0, GL_AMBIENT, Array(0.1f,0.1f,0.4f,0))
  }


  def render(picking: Boolean = false, lastHit: Option[ID] = None): Option[ID] = {
    if (picking) {
      glEnable(GL_DEPTH_TEST)
      glDisable(GL_LIGHTING)
      glDisable(GL_LIGHT0)
      glDisable(GL_TEXTURE)
      glPolygonMode(GL_FRONT, GL_FILL)
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      glLoadIdentity

      glPushMatrix

      scene.camera.setIn3D

      glBegin(GL_TRIANGLES)
      val inRange = Picking.filter(visibles.values, scene.camera.position)
      //Perf.perfed {
      inRange.foreach(e => e._2.renderPicking(e._1))
      //}
      glEnd

      glPopMatrix

      render(false, Picking.readPicking((width / 2, height / 2), inRange, {p: Pickable => p.id}))
      //lastHit
    } else {
      glEnable(GL_DEPTH_TEST)
      glEnable(GL_LIGHTING)
      glEnable(GL_LIGHT0)
      glPolygonMode(GL_FRONT, GL_FILL)
      glClear(GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      glLoadIdentity

      glPushMatrix

      scene.camera.rotate
      renderInfinitelyFar
      scene.camera.translate

      //Perf.perfed {
      renderObjects(lastHit)
      //}
      renderGrid

      glPopMatrix

      renderHud

      glFlush

      lastHit
    }
  }

  private def renderObjects(lastHit: Option[ID]) {
    glEnable(GL_LIGHTING)
    glPolygonMode(GL_FRONT, GL_FILL)
    glBegin(GL_TRIANGLES)
    lastHit match {
      case None => visibles.map(_._2.render)
      case Some(faceId) => visibles.map(o => {
        val g = o._2
        if (g.id == faceId)
          g.renderPicked((-1,0,-1))
        else
          g.render
      })}
    glEnd
    //    visible.map(_._2.render)
    //    lastHit.map( faceId => visible.get(faceId).map { g =>
    //      glDisable(GL_LIGHTING)
    //      glPolygonMode(GL_FRONT, GL_LINE)
    //      glColor3f(0,0,0)
    //      glLineWidth(2)
    //      glBegin(GL_LINES)
    //      g.renderContour
    //      glEnd
    //    })
  }

  private def renderInfinitelyFar {
    scene.translationlessRenderables.foreach(_.render)
  }

  protected def crosshair {
    glColor3f(0.7f, 0.9f, 0f)
    glBegin(GL_TRIANGLES)
    glVertex3d(0, -0.01924, -1)
    glVertex3d(0.01666, 0.0096, -1)
    glVertex3d(-0.01666, 0.0096, -1)
    glEnd
  }

  private def renderHud() {
    glDisable(GL_LIGHTING)
    glDisable(GL_DEPTH_TEST)
    glPolygonMode(GL_FRONT, GL_LINE)
    // triangle lime green crosshair
    crosshair
  }

  private def renderGrid() {
    // this creates the nice looking background.
    glDisable(GL_LIGHTING)
    glPolygonMode(GL_FRONT, GL_LINE)
    glLineWidth(1)
    glBegin(GL_TRIANGLES)
    scene.wireframes.map(_.render)
    glEnd
  }
}

object Renderer {

  // coordinates + face id
  type ID = (Int, Int, Int, Int)

  trait Renderable {
    def render
  }

  trait FaceRenderable extends Renderable {
    def renderContour
    // which other triangle this is touching
    def touching: ID
  }

  trait Pickable extends FaceRenderable with Picking.Pickable {
    def renderPicking(pickingColor: Color3B)
    def renderPicked(color: Color3B)
    def id: ID
  }

  // a renderable face (triangle)
  trait Growable extends Pickable {
    // the new set of growables obtainable from this
    def growth: Iterable[Growable]
    // the set of growables this is part of
    def trunk: Iterable[ID]
  }

  def glLightBuff(glLightEnum: Int, glConstant: Int, values: Array[Float]) {
    val buffer = BufferUtils.createFloatBuffer(values.length)
    buffer.put(values)
    buffer.flip
    glLight(glLightEnum, glConstant, buffer)
  }
}