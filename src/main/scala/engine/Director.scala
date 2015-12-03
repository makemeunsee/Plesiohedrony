package engine

import akka.actor.Actor
import akka.actor.Props
import Director._
import akka.actor.ActorRef
import engine.entity.Player
import akka.actor.Cancellable
import scala.concurrent.duration._
import scala.language.postfixOps

object Director {
  case class AcceptJoin(id: Int, worldRef: ActorRef, tickerRef: ActorRef)
  case class Welcome(msg: String)
  case class PlayerJoin(id: Int, name: String, playerRef: ActorRef)
  case class PlayerLeave(id: Int)
  case object Stop
  
  case class Timeout(id: Int)
  
  val connectionTimeout = 1 second
}

class Director extends Actor {
  
  val ticker = context.actorOf(Props[Ticker], "ticker")
  val world = context.actorOf(Props[World], "world")
  ticker ! Ticker.HighRateSynchro(world)
 
  override def preStart() {
    ticker ! Ticker.Start
  }
  
  // TODO change to PRNG
  val ids = Stream.from(0).iterator
  
  override def receive = managePlayers(players = Map.empty[Int, ActorRef], pendings = Map.empty[Int, Cancellable])

  def stopped: Receive = {
    case _ =>
  }
  
  def managePlayers(players: Map[Int, ActorRef], pendings: Map[Int, Cancellable]): Receive = {
    case Stop =>
      players foreach { _._2 ! Stop }
      world ! Stop
      ticker ! Ticker.Stop
      context.become(stopped)
    
    case Player.JoinRequest =>
      sender ! Welcome("yolo")
      val id = ids.next()
      import context.dispatcher
      val c = context.system.scheduler.scheduleOnce(connectionTimeout, self, Timeout(id))
      context.become(managePlayers(players, pendings + ((id, c))))
      sender ! AcceptJoin(id, world, ticker)
      
    case Timeout(id) =>
      context.become(managePlayers(players, pendings - id))
      
    case PlayerLeave(id) =>
      context.become(managePlayers(players - id, pendings))
      players.get(id) foreach ( ticker ! Ticker.StopSynchro(_) )
      world ! World.PlayerLeave(id)
      
    case PlayerJoin(id, name, playerRef) =>
      pendings.get(id) foreach (_.cancel())
      context.become(managePlayers(players + ((id, playerRef)), pendings - id))
      // add player to the world
      world ! World.PlayerJoin(id, playerRef, name)
  }
}