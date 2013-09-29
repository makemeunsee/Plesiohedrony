package sandbox

import org.lwjgl.opengl.{ Display, DisplayMode }
import org.lwjgl.input._
import locale.Keys
import math._
import engine.rendering.Renderer.{Renderable, ID, glLightBuff}
import engine.{Scene, Ticker, Camera}
import Ticker.Tickable
import org.lwjgl.opengl.GL11._
import scala.Some
import engine.rendering.{CaveRenderer, DefaultRenderer}

object Demo extends Tickable {
  
  val GAME_TITLE = "Plesiohedrony"
  val FRAMERATE = 60
  val SPEED = 0.01f // m/ms
  val width = 800
  val height = 600
  
  val updateDelay = 200 // ms
  
  val scene = new Scene
  val renderer = new DefaultRenderer(scene, width, height)//new CaveRenderer(scene, width, height)
    
  var finished = false

  def main(args: Array[String]) {
    var fullscreen = true
    for (arg <- args) {
      arg match {
        case "-window" =>
          fullscreen = false
      }
    }
    init(fullscreen)
    
    // fill scene
    //Shapes.heavyBubble.foreach(scene.addGrowable)
    Shapes.beautyBubble.foreach(scene.addGrowable)
    Shapes.gridFloor(20).foreach(scene.addWireframe)
    //Shapes.floor(10, 0).foreach(scene.addGrowable)
    //Shapes.at(0,0,0).foreach(scene.addGrowable)
    //Shapes.at(1,1,1).foreach(scene.addGrowable)
    Shapes.dome(10).foreach(scene.addGrowable)
    //Shapes.wall(10,10).foreach(scene.addGrowable)
    //Shapes.mcChunk.foreach(scene.addGrowable)

    val sun = new Renderable with Tickable {
      var sunAngle = 0d
      val sunDistance = 100
      val sunSize = 10
      val winterFactor = 0.5f
      val summerFactor = math.sqrt(1 - winterFactor * winterFactor).toFloat

      def tick(t: Int) {
        sunAngle -= 0.004f
      }

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
        glLightBuff(GL_LIGHT0, GL_DIFFUSE, Array(1f,1,1,0))

        glPopMatrix
      }
    }

    val moon = new Renderable {
      val moonDistance = 100
      val moonSize = 10
      def render {
        glPushMatrix

        val angle = 1.5f
        val x = moonDistance*sin(angle).toFloat
        val y = moonDistance*cos(angle).toFloat
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

    scene.translationlessRenderables.add(moon)
    scene.translationlessRenderables.add(sun)

    run
  }

  def init(fullscreen: Boolean) {
    
    // init display
    if ( fullscreen ) {
      Display.setFullscreen(true)
      Display.setDisplayMode(Display.getDesktopDisplayMode)
      renderer.width = Display.getWidth
      renderer.height = Display.getHeight
    } else {
      Display.setFullscreen(false)
      Display.setDisplayMode(new DisplayMode(width, height))
      renderer.width = width
      renderer.height = height
    }
    Display.setTitle(GAME_TITLE)
    Display.setVSyncEnabled(true)
    Display.create

    renderer.init
  }

  def cleanup() {
    Display.destroy
  }
  
  object Activity extends Enumeration {
    type Activity = Value
    val ADDING = Value("ADD")
    val REMOVING = Value("REM")
    val INFO = Value("INF")
    val NONE = Value("NON")
  }
  
  import Activity._
  
  def run() {
    Mouse.setGrabbed(true)
    
    var lastActivity = System.currentTimeMillis
    var lastUpdate = lastActivity

    Ticker.start
    
    while (!finished) {

      Display.update

      if (Keyboard.isKeyDown(Keys.FULLSCREEN)) {
        Ticker.pause
        println("switching fullscreen / window")
        Display.destroy
        init(!Display.isFullscreen)
        Ticker.unpause
      }
      val update = System.currentTimeMillis
      logic(scene.camera, update - lastUpdate)
      lastUpdate = update

      val activity = if (Mouse.isButtonDown(0)) ADDING else if (Mouse.isButtonDown(1)) REMOVING else if (Mouse.isButtonDown(2)) INFO else NONE
      val picked = renderer.render(activity != NONE)
      if ( picked != None && (System.currentTimeMillis - lastActivity) > updateDelay) {
        updateScene(picked, activity)
        lastActivity = System.currentTimeMillis
      }

      Display.sync(FRAMERATE)
    }
    Ticker.exiting = true
  }
  
  def tick(t: Int) {
    //logic(scene.camera, Ticker.tickRate)
  }

  def logic(cam: Camera, spentMS: Float) {
    // mouse events
    val dx = Mouse.getDX
    val dy = Mouse.getDY
    
    cam.pitch += dx * 0.01d
    cam.yaw += dy * 0.01d
    if (cam.yaw > Pi) cam.yaw = Pi
    if (cam.yaw < 0) cam.yaw = 0
    
//    if ( dy != 0 || dx != 0 )
//      println(s"$dx - $dy")

    // keyboard events
    import Keys._
    import Keyboard.isKeyDown

    finished = Display.isCloseRequested || isKeyDown(ESC)

    val move_forward = isKeyDown(FORWARD)
    val move_back = isKeyDown(BACK)
    val move_left = isKeyDown(LEFT)
    val move_right = isKeyDown(RIGHT)
    val move_up = isKeyDown(UP)
    val move_down = isKeyDown(DOWN)
    val move_fast = isKeyDown(RUN)

    val moveAngle = cam.pitch + {
      (move_forward, move_back, move_left, move_right) match {
        case (true, _, true, _) => -Math.PI / 4
        case (true, _, false, true) => Math.PI / 4
        case (true, _, false, false) => 0
        case (false, true, true, _) => -3* Math.PI / 4
        case (false, true, false, true) => 3*Math.PI / 4
        case (false, true, false, false) => Math.PI
        case (false, false, true, _) => -Math.PI / 2
        case (false, false, false, true) => Math.PI / 2
        case _ => 0
      }
    }

    val speed = SPEED * spentMS * { if ( move_fast ) 5 else 1 }
    val planarSpeed = if ( move_forward || move_left || move_right || move_back ) 
    	speed
      else
        0

    cam.y += Math.cos(moveAngle).toFloat * planarSpeed
    cam.x += Math.sin(moveAngle).toFloat * planarSpeed 
    cam.z += { if (move_up) speed else if (move_down) -speed else 0 }
  }
  
  def updateScene(picked: Option[ID], activity: Activity) {
    picked match {
      case None => ()
      case Some(id) => activity match {
        case ADDING => for ( pickedFace <- scene.getGrowable(id) )
          pickedFace.growth.foreach(scene.addGrowable)
        case REMOVING => for ( pickedFace <- scene.getGrowable(id) )
          pickedFace.trunk.foreach(scene.removeGrowable)
        case INFO => for ( pickedFace <- scene.getGrowable(id) )
          println(s"faceId: ${pickedFace.id}")
        case _ => ()
      }
    }
  }
}