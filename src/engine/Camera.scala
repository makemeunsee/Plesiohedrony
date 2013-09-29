package engine

import org.lwjgl.opengl.GL11.{glRotatef, glTranslatef}
import models.Honeycomb.Point3f

class Camera(var yaw: Double, var pitch: Double, var x: Float, var y: Float, var z: Float) {
  def setIn3D {
    rotate
    translate
  }
  
  def rotate {
    glRotatef(yaw.toDegrees.toFloat, -1f, 0, 0)
    glRotatef(pitch.toDegrees.toFloat, 0, 0, 1f)
  }
  
  def translate {
    glTranslatef(-x, -y, -z)
  }
  
  def position = new Point3f(x,y,z)
}