package engine

import org.lwjgl.opengl.GL11.{glRotatef, glTranslatef}
import models.Point3f

// yaw: angle between (0,0,-1) and eyeVector
// pitch: angle between plan(y = 0) and eyeVector
case class Camera(private var yaw: Double, private var pitch: Double, private var x: Float, private var y: Float, private var z: Float) {
  
  def applyToWorld() {
    rotateWorld()
    translateWorld()
  }
  
  def rotateWorld() {
    glRotatef(yaw.toDegrees.toFloat, -1f, 0, 0)
    glRotatef(pitch.toDegrees.toFloat, 0, 0, 1f)
  }
  
  def translateWorld() {
    glTranslatef(-x, -y, -z)
  }
  
  def position = new Point3f(x,y,z)
  
  def getX = x
  def getY = y
  def getZ = z
  
  def setXYZ(newX: Float, newY: Float, newZ: Float) {
    x = newX
    y = newY
    z = newZ
  }
  
  def getPitch = pitch
  def getYaw = yaw
  
  def updatePitchAndYaw(dPitch: Double, dYaw: Double) {
    pitch += dPitch
    yaw += dYaw
    if (yaw > math.Pi)
      yaw = math.Pi
    else if (yaw < 0)
      yaw = 0
    if (pitch >= 2*math.Pi)
      pitch -= 2*math.Pi
    else if (pitch < 0)
      pitch += 2*math.Pi
  }
}