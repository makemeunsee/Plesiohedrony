package engine

import collection.mutable.ArrayBuffer
import akka.actor.Actor
import akka.actor.ActorRef
import Ticker._
import scala.collection.mutable.Set
import akka.actor.Cancellable
import scala.concurrent.duration._

object Ticker {
  case object Start
  case object Stop
  case object Pause
  case object Unpause
  case object InternalTick
  case class Tick(t: Long)
  case class HighRateSynchro(a: ActorRef)
  case class LowRateSynchro(a: ActorRef)
  case class StopSynchro(a: ActorRef)
  
  val highTickRate = 20.millis
  val lowTickRateRatio = 50l // highTickRates
}

class Ticker extends Actor {

  private val highTickables = Set.empty[ActorRef]
  private val lowTickables = Set.empty[ActorRef]
  
  import context.dispatcher
  private def newTickingTask(initialTick: Long): Cancellable = {
    context.system.scheduler.schedule(highTickRate, highTickRate, self, InternalTick)
  }
  
  private def running(tickingTask: Cancellable, lastTick: Long): Receive = {
    case InternalTick =>
      val t = lastTick + 1
      context.become(running(tickingTask, t) orElse always)
      highTickables map ( _ ! Tick(t) )
      if (t % lowTickRateRatio == 0)
        lowTickables map ( _ ! Tick(t) )
      
    case Stop =>
      tickingTask.cancel
      context.become(stopped orElse always)
      
    case Pause =>
      tickingTask.cancel
      context.become(paused(lastTick) orElse always)
  }
  
  private def paused(lastTick: Long): Receive = {
    case Unpause => context.become(running(newTickingTask(lastTick), lastTick) orElse always)
    case Stop => context.become(stopped)
  }
  
  private def stopped: Receive = {
    case Start => context.become(running(newTickingTask(0l), 0l) orElse always)
  }
  
  private def always: Receive =  {
    case HighRateSynchro(actorRef) => highTickables.add(actorRef)
    case LowRateSynchro(actorRef) => lowTickables.add(actorRef)
    case StopSynchro(actorRef) => highTickables.remove(actorRef) ; lowTickables.remove(actorRef)
  }
  
  override def receive = stopped orElse always
}
