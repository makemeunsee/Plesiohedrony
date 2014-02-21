package client

import akka.actor.Actor
import akka.actor.Props
import engine.Director
import perf.Perf
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import engine.entity.Player
import akka.actor.ActorRef
import akka.event.LoggingReceive

object StandAlone {
  
  val cfg = ConfigFactory.load
  val system = ActorSystem("plesio-system", cfg.getConfig("standalone"))
  
  def main(args: Array[String]) {
    system.actorOf(Props[StandAlone], name = "standaloneclient")
  }
}

import StandAlone._

class StandAlone extends Actor {

  val localDirector = system.actorOf(Props[Director], name = "director")
  var localPlayer = Option.empty[ActorRef]
  
  override def preStart() {
    // for now just create a new local player
    localDirector ! Player.JoinRequest
  }
  
  override def receive = {
    case Director.AcceptJoin(id, world, ticker) => {
      val name = "alice"
      localPlayer = Some(context.actorOf(Props(classOf[Player], id, name, world, ticker), s"player_$name"))
      sender ! Director.PlayerJoin(id, name, localPlayer.get)
    }
    case Player.Stop => Perf.printResults ; localDirector ! Director.Stop ; system.stop(self) ; system.shutdown
  }
  
  // TODO: client should be a GUI to set up a game or find existing remote games

}