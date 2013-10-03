package locale

import java.awt.im.InputContext
import org.lwjgl.input.Keyboard
import Keyboard._

object Keys {
  
  private val inputLocale = InputContext.getInstance.getLocale
  
  private val defaults = (KEY_W, KEY_S, KEY_A, KEY_D, KEY_LSHIFT, KEY_SPACE, KEY_LCONTROL, KEY_E, KEY_F11, KEY_ESCAPE)
  
  private val asTuple = inputLocale.toString.toLowerCase match {
    // TODO find more reliable way to detect keyboard layout...
    //case "fr_ch" => defaults
    //case "fr_fr" => (KEY_Z, KEY_S, KEY_Q, KEY_D, KEY_LSHIFT, KEY_SPACE, KEY_LCONTROL, KEY_E, KEY_F11, KEY_ESCAPE)
    case _ => defaults
  }
  
  val FORWARD = asTuple._1
  val BACK = asTuple._2
  val LEFT = asTuple._3
  val RIGHT = asTuple._4
  val RUN = asTuple._5
  val UP = asTuple._6
  val DOWN = asTuple._7
  val USE = asTuple._8
  val FULLSCREEN = asTuple._9
  val ESC = asTuple._10

  val allKeys = Array(FORWARD, BACK, LEFT, RIGHT, RUN, UP, DOWN, USE, FULLSCREEN, ESC)
}