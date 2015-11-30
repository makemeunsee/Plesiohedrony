package engine.rendering

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._

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

abstract class Renderer(var width: Int, var height: Int) {
  def init(): Unit
  def render(picking: Boolean = false, lastHit: Option[ID] = None): Option[ID]
}