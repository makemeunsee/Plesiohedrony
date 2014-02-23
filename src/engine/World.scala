package engine

import akka.actor.Actor
import sandbox.Shapes
import engine.physics.SpherePhysics
import akka.actor.ActorRef
import engine.entity.Player
import World._
import models.Point3f
import engine.entity.Activity._
import client.Configuration._
import akka.event.LoggingReceive
import engine.rendering.Pickable
import models.container.Boundable
import engine.entity.PlayerAvatar

object World {
  case class AddViewer(viewer: ActorRef)
  case class RemoveViewer(viewer: ActorRef)
  case class SunAngle(newSunAngle: Float)
  
  case class FaceInfo(info: String)
  
  case class PlayerJoin(id: Int, entity: ActorRef, name: String)
  case class PlayerLeave(id: Int)
  
  case class Position(id: Int, at: Point3f)
  case class PlayerList(idsAndNames: Map[Int, String])
  
  case class VisibleAdded(e: Pickable)
  case class VisibleRemoved(e: Pickable)
  
  
  // sun goes one round / minute
  val sunAngleStep = Math.PI.toFloat * 2 / 60000f * Ticker.highTickRate.toMillis
  
  val actionThreshold = propPlayerActionInterval / Ticker.highTickRate.toMillis // one action at most every 150ms
  
  val startPosition = new Point3f(0,0,0)
}

class World extends Actor {

  import scala.collection.mutable.Set
  val viewers = Set.empty[ActorRef]
  
  Shapes.scale = propScale
  val physics = new SpherePhysics
  var scene = new Scene(Shapes.scale == 1f, physics, viewers)
  
  var sunAngle = 0f
  var lastTick = 0l
    
  override def preStart {
    setupScene
  }
  
  override def receive = existWith(Map.empty, Map.empty)
  
  def existWith(players: Map[Int, PlayerAvatar], actions: Map[Int, Long]): Receive = {
    // set up / tear down viewers
    case AddViewer(viewer) => registerViewer(viewer)
    case RemoveViewer(viewer) => viewers -= viewer
    
    case Director.Stop => viewers.clear
    
    // universal clock
    case Ticker.Tick(t) =>
      sunAngle -= sunAngleStep
      viewers foreach ( _ ! SunAngle(sunAngle) )
      handleWorldLife(t)
      lastTick = t
    
    // player actions
    case Player.Movement(id, vec) =>
      players.get(id) map { p =>
	    val allowed = physics.playerMove(p, vec)
        p.move(allowed)
        players.values.map { _.ref ! Position(id, p.getXYZ) }
      }

    case Player.FaceAction(playerId, faceId, a) =>
      for ( lastActionTick <- actions.get(playerId).orElse(Some(0l))
            if (lastActionTick + actionThreshold < lastTick) ;
            e <- scene.getElement(faceId) ) {
        context.become(existWith(players, actions + ((playerId, lastTick))))
        a match {
          case ADDING => scene = scene.addObject(e.growth)
          case REMOVING => scene = scene.removeObject(e.trunk)
          case INFO => sender ! FaceInfo(s"faceId: $faceId exists!")
          case _ => ()
        }
      }
      
    case Player.LookingAt(playerId, pitch, yaw) =>
      players.values.filter(_.id != playerId).map { _.ref ! Player.LookingAt(playerId, pitch, yaw) }
    
    case PlayerJoin(id, playerRef, name) =>
      val newPlayers = players + ((id, new PlayerAvatar(id, playerRef, name)))
      val idsAndNames = newPlayers map { case (id, p) => (id, p.name) }
      context.become(existWith(newPlayers, actions))
      newPlayers map { case (_, p) =>
        p.ref ! PlayerList(idsAndNames)
        p.ref ! Position(id, startPosition)
      }
      players.values map { p => playerRef ! Position(p.id, p.getXYZ) }
    
    case PlayerLeave(id) =>
      println(s"leaving this world: $id")
      val newPlayers = players - id
      val idsAndNames = newPlayers map { case (id, p) => (id, p.name) }
      context.become(existWith(newPlayers, actions - id))
      newPlayers map { case (_, p) => p.ref ! PlayerList(idsAndNames) }

  }
  
  private def registerViewer(newViewer: ActorRef) {
    viewers += newViewer
    scene.onElements( 
        { e => newViewer ! VisibleAdded(e) },
        { (_, _) => () }) // dont care about touching elements
  }
    
  private def setupScene {
    import perf.Perf.perfed
      
      // fill scene
//      perfed("heavyBubble") {
//       scene = scene.addObject(Shapes.heavyBubble)
//      }
//      perfed("beautybubble") {
//       scene = scene.addObject(Shapes.beautyBubble)
//      }

//    scene.addObject(Shapes.floor(10, 0))
    scene = scene.addObject(Shapes.at(1,1,1))
                 .addObject(Shapes.at(2,3,4))
                 .addObject(Shapes.at(5,6,7))
                 .addObject(Shapes.at(8,6,4))
//    perfed("dome") {
//      scene = scene.addObject(Shapes.dome(10))
//    }
//     scene.addObject(Shapes.wall(10,10))
//    perfed("mcchunk") {
//      Shapes.mcChunk.foreach(scene.addElement)
//    }
  }
  
  val growChance = propGrowthRate
  val decayChance = propDecayRate
  val lifeRate = 10000
  
  def handleWorldLife(tick: Long) {
    val mod = tick % 5
    if( propGrowth && mod == 0 ) {
      for ( e <- scene.elements.values
          if ( math.random * lifeRate < growChance ) )
        scene = scene.addObject(e.growth)
    } else if ( propDecay && mod == 4 ) {
      for ( e <- scene.elements.values
          if ( math.random * lifeRate < decayChance ) )
        scene = scene.removeObject(e.trunk)
    }
  }
}