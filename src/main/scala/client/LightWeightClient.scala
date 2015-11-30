package client

import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Props
import Configuration._

object LightWeightClient {

  val cfg = ConfigFactory.load
  val system = ActorSystem("thinclient", cfg.getConfig("remote"))
  val remotePath = s"akka.tcp://plesio-system@$propRemoteHost:$propRemotePort/user/director"
    
  def main(args: Array[String]): Unit = {
    system.actorOf(Props(classOf[DirectorProxy], remotePath), "lookupActor")
  }
  
}

import scala.concurrent.duration._
import akka.actor.Actor
import akka.actor.ActorIdentity
import akka.actor.ActorRef
import akka.actor.Identify
import akka.actor.ReceiveTimeout
import engine.Director
import engine.entity.Player
import perf.Perf

class DirectorProxy(path: String) extends Actor {

  sendIdentifyRequest()

  def sendIdentifyRequest(): Unit = {
    context.actorSelection(path) ! Identify(path)
    import context.dispatcher
    context.system.scheduler.scheduleOnce(3.seconds, self, ReceiveTimeout)
  }

  override def receive = identifying

  def identifying: Actor.Receive = {
    case ActorIdentity(`path`, Some(actor)) =>
      context.watch(actor)
      context.become(active(actor, Option.empty))
      self ! Player.JoinRequest
      
    case ActorIdentity(`path`, None) => println(s"Remote actor not available: $path")
    
    case ReceiveTimeout              => sendIdentifyRequest()
    
    case any                         => println("Not ready yet")
  }
  
  def active(actor: ActorRef, identity: Option[(Int, ActorRef)]): Actor.Receive = {
    case Player.JoinRequest => actor ! Player.JoinRequest
    
    case Director.Welcome(msg) => println(msg)
    
    case Director.AcceptJoin(id, world, ticker) =>
      val name = Configuration.propOrElse("playerName", "bob")
      val player = context.actorOf(Props(classOf[Player], id, name, world, ticker), s"player_${name}_$id")
      context.become(active(actor, Some((id, player))))
      sender ! Director.PlayerJoin(id, name, player)
      
    case Player.Stop =>
      identity foreach { case (id, player) => actor ! Director.PlayerLeave(id) }
      endGame()
      
    case Director.Stop => endGame()
  }
  
  def endGame() {
    Perf.printResults()
    context.stop(self)
    context.system.terminate()
  }
}