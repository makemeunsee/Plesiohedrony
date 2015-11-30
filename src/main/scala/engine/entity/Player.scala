package engine.entity

import engine.Camera
import engine.World.LocalPlayer
import engine.rendering.Colors.Color3B
import models.container.{Boundable, Bounds}
import scala.math.Pi
import models.Point3f
import akka.actor.Actor
import akka.actor.ActorRef
import Player._
import akka.actor.Props
import engine.rendering.{Colors, ID}
import engine.World
import client.Configuration
import engine.Ticker
import engine.Director
import ui.ingame.UI3D


import ui.ingame.Actions

object Player {

  sealed trait Activity
  case object ADDING extends Activity
  case object REMOVING extends Activity
  case object INFO extends Activity
  case object COLORING extends Activity
  case object NONE extends Activity

  case object Stop
  case object JoinRequest
  case class Update(playerId: Int, color: Color3B, name: String)
  case class Movement(playerId: Int, move: Point3f)
  case class LookingAt(playerId: Int, pitch: Float, yaw: Float)
  case class FaceAction(playerId: Int, faceId: ID, activity: Activity)
  
  val no_move = new Point3f(0,0,0)
}

// TODO: split player into player + mailman / world proxy

class Player(id: Int, name: String, world: ActorRef, ticker: ActorRef) extends Actor {
  val avatar = new PlayerAvatar(id, self, name)
  val ui = context.actorOf(Props(classOf[UI3D], avatar, name), "ui3d")

  override def preStart() {
    world ! World.AddViewer(ui)
    ticker ! Ticker.HighRateSynchro(self)
    ui ! LocalPlayer(avatar)
  }
  
  override def receive = playing(no_move, None, NONE, 0)
    
  def playing(movement: Point3f, pick: Option[ID], activity: Activity, colorId: Int): Receive = {
    // director stop the game
    case Director.Stop => context.parent ! Director.Stop
    // player exits the game
    case Actions.ExitRequest => context.parent ! Stop
    
    // collect actions
    case mov: Actions.Move =>
      val vec = handleMovement(mov)
      context.become(playing(movement + vec, pick, activity, colorId))
      avatar.move(vec)
    case Actions.Picked(faceId, newActivity) =>
      context.become(playing(movement, Some(faceId), newActivity, colorId))

    case Actions.ChangeColorNext =>
      val newColorId = Colors.nextId(colorId)
      avatar.color = Colors(newColorId)
      ui ! LocalPlayer(avatar)
      context.become(playing(movement, pick, activity, newColorId))
    case Actions.ChangeColorPrevious =>
      val newColorId = Colors.prevId(colorId)
      avatar.color = Colors(newColorId)
      ui ! LocalPlayer(avatar)
      context.become(playing(movement, pick, activity, newColorId))

    case pos: World.Position => if ( pos.id != id) ui ! pos else avatar.setXYZ(pos.at.x, pos.at.y, pos.at.z)
    case look: LookingAt => if ( look.playerId != id) ui ! look
    case l: World.PlayerList => ui ! l
    case World.FaceInfo(s) => println(s)
    
    // send actions at each tick
    case Ticker.Tick(_) =>
      world ! Update(id, avatar.color, name)
      if ( movement != no_move ) world ! Movement(id, movement)
      pick foreach ( world ! FaceAction(id, _, activity) )
      world ! LookingAt(id, avatar.getPitch.toFloat, avatar.getYaw.toFloat)
      context.become(playing(no_move, None, NONE, colorId))

  }

  private def handleMovement(a: Actions.Move): Point3f = {

    val moveAngle = avatar.getPitch + {
      (a.forward, a.backward, a.left, a.right) match {
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

    val speed = Configuration.propPlayerSpeed * a.duration * { if ( a.speed ) 5 else 1 }
    val planarSpeed = if ( a.forward || a.backward || a.left || a.right ) 
        speed
      else
        0

    if ( planarSpeed != 0 || a.up || a.down ) {
      val moveY = Math.cos(moveAngle).toFloat * planarSpeed
      val moveX = Math.sin(moveAngle).toFloat * planarSpeed 
      val moveZ = if (a.up) speed else if (a.down) -speed else 0
      
      new Point3f(moveX, moveY, moveZ)
    } else
      no_move
  }
}

class PlayerAvatar(val id: Int, val ref: ActorRef, val name: String)
    extends Camera(Pi / 2d,0,0,0,0) with Boundable {

  val radius = 0.5f
  val squareRadius = radius*radius

  var color: Color3B = Colors(0)
  
  def move(mv: Point3f) {
    setXYZ(getX + mv.x, getY + mv.y, getZ + mv.z)
  }
  
  def getXYZ = new Point3f(getX, getY, getZ)
  
  // player bounds are a sphere
  def within(bounds: Bounds) = perf.Perf.perfed("player within") {
    val position = (bounds._1 <= getX, bounds._2 > getX,
      bounds._3 <= getY, bounds._4 > getY,
      bounds._5 <= getZ, bounds._6 > getZ)
    position match {
      case (true, true, true, true, true, true)  => true // fully within
      // closer to a face
      case (false, true, true, true, true, true) => bounds._1 - getX < radius
      case (true, false, true, true, true, true) => getX - bounds._2 < radius
      case (true, true, false, true, true, true) => bounds._3 - getY < radius
      case (true, true, true, false, true, true) => getY - bounds._4 < radius
      case (true, true, true, true, false, true) => bounds._5 - getZ < radius
      case (true, true, true, true, true, false) => getZ - bounds._6 < radius
      // closer to an edge
      case (false, true, false, true, true, true) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._3)*(getY - bounds._3) < squareRadius
      case (false, true, true, false, true, true) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._4)*(getY - bounds._4) < squareRadius
      case (true, false, false, true, true, true) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._3)*(getY - bounds._3) < squareRadius
      case (true, false, true, false, true, true) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._4)*(getY - bounds._4) < squareRadius
      
      case (false, true, true, true, false, true) => (getX - bounds._1)*(getX - bounds._1) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (false, true, true, true, true, false) => (getX - bounds._1)*(getX - bounds._1) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (true, false, true, true, false, true) => (getX - bounds._2)*(getX - bounds._2) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, false, true, true, true, false) => (getX - bounds._2)*(getX - bounds._2) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      
      case (true, true, false, true, false, true) => (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, true, false, true, true, false) => (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (true, true, true, false, false, true) => (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, true, true, false, true, false) => (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      // closer to a vertex
      case (false, true, false, true, false, true) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (false, true, false, true, true, false) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (false, true, true, false, false, true) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (false, true, true, false, true, false) => (getX - bounds._1)*(getX - bounds._1) + (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (true, false, false, true, false, true) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, false, false, true, true, false) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._3)*(getY - bounds._3) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      case (true, false, true, false, false, true) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._5)*(getZ - bounds._5) < squareRadius
      case (true, false, true, false, true, false) => (getX - bounds._2)*(getX - bounds._2) + (getY - bounds._4)*(getY - bounds._4) + (getZ - bounds._6)*(getZ - bounds._6) < squareRadius
      
      case _ => throw new Error(s"Oopps weird: $position")
    }
  }
  
  override def toString = s"Player $name $id @ $getX - $getY - $getZ"
}