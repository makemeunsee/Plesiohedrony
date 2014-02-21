package client

import java.io.IOException

object Configuration {
  private val cfg: java.util.Properties = {
    val props = new java.util.Properties
    val stream = getClass getResourceAsStream "../config.properties"
    if (stream ne null)
      try     { props load stream }
      finally {
        try     { stream.close }
        catch   { case e: IOException => e.printStackTrace}
      }
    props
  }

  private def propOrElse(name: String, alt: String): String = cfg.getProperty(name, alt)

  val propFullscreen = propOrElse("fullscreen", "false").toBoolean
  val propWidth = propOrElse("width", "800").toInt
  val propHeight = propOrElse("height", "600").toInt
  val propFramerate = propOrElse("framerate", "60").toInt
  val propHoneycomb = Class.forName(s"models.${propOrElse("honeycomb", "BitruncatedCubic")}Honeycomb")
  val propGrowth = propOrElse("growth", "true").toBoolean
  val propGrowthRate = propOrElse("growthRate", "1.3").toFloat
  val propDecay = propOrElse("decay", "true").toBoolean
  val propDecayRate = propOrElse("decayRate", "1.0").toFloat
  val propScale = propOrElse("scale", "1").toFloat
  val propPlayerActionInterval = propOrElse("playerActionInterval", "200").toInt
  val propPlayerSpeed = propOrElse("playerSpeed", "2").toFloat / 1000f
  val propCollision = propOrElse("collision", "true").toBoolean
}
