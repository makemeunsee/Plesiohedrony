package engine.rendering

import engine.Scene
import org.lwjgl.opengl.GL11._

class CaveRenderer(scene: Scene, w: Int, h: Int)
    extends DefaultRenderer(scene, w, h) {
  override def init(): Unit = {
    super.init()
    glFrontFace(GL_CW)
  }

  override protected def crosshair(): Unit = {
    glColor3f(0.7f, 0.9f, 0f)
    glBegin(GL_TRIANGLES)
    glVertex3d(0.01666, 0.0096, -1)
    glVertex3d(0, -0.01924, -1)
    glVertex3d(-0.01666, 0.0096, -1)
    glEnd
  }
}
