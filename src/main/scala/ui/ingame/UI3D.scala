package ui.ingame

import akka.actor.Actor
import engine.rendering._
import engine.Camera
import client.Configuration.{propWidth, propHeight, propFullscreen, propFramerate}
import org.lwjgl.opengl.{Display, DisplayMode}
import org.lwjgl.input.Keyboard
import engine.World
import org.lwjgl.input.Mouse
import Actions._
import UI3D._
import engine.World._
import engine.entity.Player
import engine.entity.Player.Activity
import engine.entity.Player.NONE
import engine.entity.Player.ADDING
import engine.entity.Player.REMOVING
import engine.entity.Player.COLORING

object Actions {
  case class Picked(id: ID, a: Activity)
  object ExitRequest
  case class Move(forward: Boolean,
      backward: Boolean,
      left: Boolean,
      right: Boolean,
      up: Boolean,
      down: Boolean,
      speed: Boolean,
      duration: Long)
  object ChangeColorNext
  object ChangeColorPrevious
} 

private object UI3D {
  case object Ready
  case object NextInput
  case object FullscreenSwitch
}

class UI3D(camera: Camera, name: String) extends Actor {

  // viewer state
  private val renderer: DefaultRenderer = new DefaultRenderer(camera, propWidth, propHeight)
  //new CaveRenderer(camera, scene, propWidth, propHeight, visibles)
  private var finished = false
  
  // viewer / controller interacttion
  private var mode: Activity = NONE
  private var fullscreenSwitchRequested = false
  
  // controller state
  var lastMouseUpdate = System.currentTimeMillis
  var lastKeyboardUpdate = System.currentTimeMillis
  
  override def preStart() {
    // start rendering loop
    new Thread {
      override def run() {
        init(propFullscreen)
    
        while (!finished) {

          Display.update()
  
          if (fullscreenSwitchRequested) {
            println("switching fullscreen / window")
            Display.destroy()
            init(!Display.isFullscreen)
          }
          
          val renderingMode = mode
          val picked = renderer.render(renderingMode != NONE)
          picked foreach ( context.parent ! Picked(_, renderingMode) )
  
          Display.sync(propFramerate)
        }
        
        Display.destroy()
        Mouse.setGrabbed(false)
        println("3d render thread exit")
      }
    }.start()
  }
  
  override def postStop() {
    // ensure rendering thread exits
    finished = true
  }
  
  import scala.language.reflectiveCalls
  def unmissable: Receive = {
    case VisibleRemoved(e) => renderer.visibles -= e.id
    case VisibleUpdated(e) => renderer.visibles += ((e.id, e))
    case VisibleAdded(e) => renderer.visibles += ((e.id, e))
    case LocalPlayer(avatar) => renderer.localPlayer = Some(avatar)

    case PlayerList(list) =>
      renderer.players = renderer.players.filter { case (oId, _) => list.contains(oId) }
      list foreach { case (oId, oName) => println(s"$oName, $oId") }
      
    case Position(oId, at) => renderer.players.get(oId) match {
      case Some(state) => renderer.players += ( (oId, state.copy(_1 = at)) )
      case _ => renderer.players += ( (oId, (at, 0, 0)) )
    }
      
    case Player.LookingAt(oId, pitch, yaw) => renderer.players.get(oId) match {
      case Some(state) => renderer.players += ( (oId, state.copy(_2 = pitch, _3 = yaw)) )
      case _ => // discard
    }
      
  }
  
  def handleInputs: Receive = {
    case NextInput =>
      lastMouseUpdate = control(lastMouseUpdate, inputs)
    case FullscreenSwitch =>
      context.become(stopped)
      fullscreenSwitchRequested = true
  }
  
  def missable: Receive = {
    case World.SunAngle(angle) => renderer.sunAngle = angle
  }
  
  def started = unmissable orElse missable orElse handleInputs
  
  def waiting: Receive = {
    case Ready =>
      context.become(started)
      self ! NextInput
  }
  
  def stopped = unmissable orElse waiting
  
  override def receive = stopped

  def init(fullscreen: Boolean) {
      
    // init display
    if ( fullscreen ) {
      Display.setFullscreen(true)
      Display.setDisplayMode(Display.getDesktopDisplayMode)
      renderer.width = Display.getWidth
      renderer.height = Display.getHeight
    } else {
      Display.setFullscreen(false)
      Display.setDisplayMode(new DisplayMode(propWidth, propHeight))
      renderer.width = propWidth
      renderer.height = propHeight
    }
    Display.setTitle(s"Plesiohedrony $name")
    Display.setVSyncEnabled(true)
    Display.create()
    
    Mouse.setGrabbed(true)
    
    fullscreenSwitchRequested = false

    self ! Ready
    
    renderer.init()
  }
  
  def control(lastUpdate: Long, function: (Long => Unit)): Long = {
    val update = System.currentTimeMillis
    function(update - lastUpdate)
    update
  }
  
  def inputs(spentMS: Long) {
    import locale.Keys._
    import Keyboard.isKeyDown

    val wheel = Mouse.getDWheel
    
    if (Display.isCloseRequested || isKeyDown(ESC)) {
      context.parent ! ExitRequest
    } else if (Keyboard.isKeyDown(FULLSCREEN)) {
      self ! FullscreenSwitch
    } else if (Keyboard.isKeyDown(USE)) {
      // toggle mouse grab
      Mouse.setGrabbed(!Mouse.isGrabbed)
    } else if (wheel < 0) {
      context.parent ! ChangeColorNext
    } else if (wheel > 0) {
      context.parent ! ChangeColorPrevious
    } else {
      val fwd = isKeyDown(FORWARD)
      val bwd = isKeyDown(BACK)
      val left = isKeyDown(LEFT)
      val right = isKeyDown(RIGHT)
      val up = isKeyDown(UP)
      val down = isKeyDown(DOWN)
      val fast = isKeyDown(RUN)
      if ( fwd || bwd || left || right || up || down ) {
        context.parent ! Move(fwd,
              bwd,
              left,
              right,
              up,
              down,
              fast,
              spentMS)
      }
      val activity =
        if (Mouse.isButtonDown(0))
          ADDING
        else if (Mouse.isButtonDown(1))
          REMOVING
        else if (Mouse.isButtonDown(2))
          COLORING
        else
          NONE
      if (activity != mode) {
        mode = activity
      }
    }
//  }
//  
//  def look(spentMS: Long) {
//    // mouse events
    val dx = Mouse.getDX
    val dy = Mouse.getDY
    
    if (dx != 0 || dy != 0) {
      camera.updatePitchAndYaw(dx*0.01d, dy*0.01d)
    }

    if (spentMS < 5)
      Thread.sleep(5-spentMS)

    self ! NextInput
  }

}