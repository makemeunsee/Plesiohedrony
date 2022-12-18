package engine.rendering

import engine.{Scene}
import Renderer._
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.Display
import perf.Perf.perfed
import models.Point3f
import models.container.{Boundable, Octree}
import scala.collection.mutable.ArrayBuffer
import util.Collections.successivePairsCycling
import engine.Element

abstract class Renderer(val scene: Scene, var width: Int, var height: Int) {
  def init(): Unit
  def render(picking: Boolean = false, lastHit: Option[ID] = None): Option[ID]
}

class DefaultRenderer(scene: Scene, w: Int, h: Int)
    extends Renderer(scene, w, h) {

  protected def visibles = scene.visible

  def init(): Unit = {

    println(
      s"OpenGL version: ${glGetString(GL_VERSION)} from ${glGetString(GL_VENDOR)}"
    )
    println("init gl")

    glEnable(GL_DEPTH_TEST)
    glEnable(GL_LIGHTING)
    glEnable(GL_LIGHT0)
    glEnable(GL_COLOR_MATERIAL)
    glEnable(GL_CULL_FACE)
    glEnable(GL_NORMALIZE)
    // glEnable(GL_LINE_SMOOTH)
    glPolygonMode(GL_FRONT, GL_FILL)

    // no global ambient light
    glLightModelBuff(GL_LIGHT_MODEL_AMBIENT, Array(0f, 0, 0, 0))

    // glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE)

    val nearest = 0.2f
    val v =
      nearest * Display.getDisplayMode.getWidth.toDouble / Display.getDisplayMode.getHeight.toDouble
    println(s"v:$v")

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity

    glFrustum(-v, v, -nearest, nearest, nearest, 110)
    glMatrixMode(GL_MODELVIEW)

    // sun light color
    // glLightBuff(GL_LIGHT0, GL_DIFFUSE, Array(1f,1f,0f,0))

    // 'moonlight' ambient
    glLightBuff(GL_LIGHT0, GL_AMBIENT, Array(0.2f, 0.3f, 0.4f, 0))
  }

  def render(picking: Boolean = false, lastHit: Option[ID] = None): Option[ID] =
    perfed("render") {
      if (picking) {
        glDisable(GL_LIGHTING)
        glDisable(GL_LIGHT0)
        glDisable(GL_TEXTURE)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity

        glPushMatrix

        scene.camera.applyToWorld()

        glBegin(GL_TRIANGLES)
        val inRange = perfed("visible") {
          Picking.filter(visibles.values, scene.camera.position)
        }
        inRange.foreach { case (pickingColor, g) =>
          render(g, pickingColor, g.normal)
        }
        glEnd

        glPopMatrix

        glEnable(GL_LIGHT0)
        glEnable(GL_LIGHTING)
        render(
          false,
          Picking.readPicking(
            (width / 2, height / 2),
            inRange,
            { (p: Pickable) => p.id }
          )
        )
        // lastHit
      } else {
        glClear(
          GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT | GL_DEPTH_BUFFER_BIT
        )
        glLoadIdentity

        glPushMatrix

        scene.camera.rotateWorld()
        renderInfinitelyFar()
        scene.camera.translateWorld()

        renderObjects(lastHit)
        renderGrid()
        // debug: view octree leaves
        renderOctree()

        glPopMatrix

        renderHud()

        glFlush

        lastHit
      }
    }

  protected def render(r: FaceRenderable): Unit = {
    render(r, r.color, r.normal)
  }

  protected def renderPicked(r: Pickable): Unit = {
    render(r, r.pickedColor, r.normal)
  }

  protected def renderLine(r: Renderable): Unit = {
    glColor3ub(r.color._1, r.color._2, r.color._3)
    successivePairsCycling(r.toContour.toList).map { case (p1, p2) =>
      glVertex3f(p1.x, p1.y, p1.z)
      glVertex3f(p2.x, p2.y, p2.z)
    }
  }

  protected def crosshair(): Unit = {
    glColor3f(0.7f, 0.9f, 0f)
    glBegin(GL_TRIANGLES)
    glVertex3d(0, -0.01924, -1)
    glVertex3d(0.01666, 0.0096, -1)
    glVertex3d(-0.01666, 0.0096, -1)
    glEnd
  }

  protected def drawTriangle(t: (Point3f, Point3f, Point3f)): Unit = {
    glVertex3f(t._1.x, t._1.y, t._1.z)
    glVertex3f(t._2.x, t._2.y, t._2.z)
    glVertex3f(t._3.x, t._3.y, t._3.z)
  }

  private def render(r: Renderable, c: Color3B, n: Point3f): Unit = {
    glColor3ub(c._1, c._2, c._3)
    glNormal3f(n.x, n.y, n.z)
    r.toTriangles.map(drawTriangle)
  }

  private def renderObjects(lastHit: Option[ID]): Unit = {
    glEnable(GL_LIGHTING)
    glPolygonMode(GL_FRONT, GL_FILL)
    glEnable(GL_POLYGON_OFFSET_FILL)
    glPolygonOffset(1f, 1f)
    glBegin(GL_TRIANGLES)
    lastHit match {
      case None =>
        visibles.map { case (id, v) =>
          // debug code to see collisions
          if (scene.player.colliding.contains(v))
            render(v, (-1, 0, 0), v.normal)
          else
            render(v)
        }
      case Some(faceId) =>
        visibles.map(o => {
          val g = o._2
          if (g.id == faceId)
            renderPicked(g)
          else
            render(g)
        })
    }
    glEnd
    // show edges of polyhedrons.
//    glEnable(GL_LINE_SMOOTH)
//    glHint( GL_LINE_SMOOTH_HINT, GL_NICEST )
    // glLineWidth(1.5f)
    glDisable(GL_LIGHTING)
    glDisable(GL_POLYGON_OFFSET_FILL)
    glPolygonMode(GL_FRONT, GL_LINE)
    glColor3f(1, 1, 1)
    glBegin(GL_LINES)
    visibles.map { g =>
      renderLine(g._2)
    }
    glEnd
  }

  private def renderInfinitelyFar(): Unit = {
    scene.translationlessRenderables.foreach(_.render())
  }

  private def renderHud(): Unit = {
    glDisable(GL_LIGHTING)
    glDisable(GL_DEPTH_TEST)
    glPolygonMode(GL_FRONT, GL_LINE)
    // triangle lime green crosshair
    crosshair()
    glEnable(GL_LIGHTING)
    glEnable(GL_DEPTH_TEST)
    glPolygonMode(GL_FRONT, GL_FILL)
  }

  private def renderGrid(): Unit = {
    // this creates the nice looking background.
    glDisable(GL_LIGHTING)
    glLineWidth(1)
    glBegin(GL_LINES)
    scene.wireframes.map(renderLine)
    glEnd
  }

  private def renderOctree(octree: Octree[Element]): Unit = {
    for (quad <- octree.quads; pair <- successivePairsCycling(quad)) {
      if (octree.depth == Octree.maxDepth) {
        if (!octree.empty) {
//          if(octree.values.contains(scene.player))
//            glColor3f(1f,0.3f,0.3f)
//          else
          glColor3f(0.3f, 0.3f, 0.3f)
          glVertex3f(pair._1.x, pair._1.y, pair._1.z)
          glVertex3f(pair._2.x, pair._2.y, pair._2.z)
        }
      }
    }
    for (kids <- octree.children; kid <- kids)
      renderOctree(kid)
  }

  private def renderOctree(): Unit = {
    glDisable(GL_LIGHTING)
    glLineWidth(1)
    glBegin(GL_LINES)
    renderOctree(scene.octree)
    glEnd
  }
}

object Renderer {

  def glLightBuff(
      glLightEnum: Int,
      glConstant: Int,
      values: Array[Float]
  ): Unit = {
    val buffer = BufferUtils.createFloatBuffer(values.length)
    buffer.put(values)
    buffer.flip
    glLight(glLightEnum, glConstant, buffer)
  }

  def glLightModelBuff(glLightModelEnum: Int, values: Array[Float]): Unit = {
    val buffer = BufferUtils.createFloatBuffer(values.length)
    buffer.put(values)
    buffer.flip
    glLightModel(glLightModelEnum, buffer)
  }
}
