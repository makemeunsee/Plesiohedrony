package engine

import engine.rendering.{Color3B, ColorMutation, Growable, ID}
import perf.Perf.perfed
import models.container.Boundable
import engine.physics.{Physics, Collidable}
import scala.collection.immutable.HashMap
import scala.collection.mutable
import akka.actor.ActorRef
import engine.World.{VisibleAdded, VisibleRemoved}

trait Element extends Boundable with Growable[Element] with ColorMutation

case class Scene[A <: Collidable[A]](hideTouching: Boolean,
                                     physics: Physics[A],
                                     viewers: mutable.Set[ActorRef],
                                     elements: Map[ID, Element] = new HashMap[ID, Element])
                                     (implicit elementToA: Element => A) {

  def addObject(o: Iterable[Element]): Scene[A] = {
    o.foldLeft(this) { case (lastScene, element) =>
      lastScene.addElement(element)
    }
  }
  
  def removeObject(o: Iterable[ID]): Scene[A] = {
    o.flatMap(getElement).foldLeft(this) { case (lastScene, element) =>
      lastScene.removeElement(element)
    }
  }

  def colorObject(e: Element, color: Color3B): Unit = {
    e.setColor(color._1, color._2, color._3)
  }
  
  private def addElement(e: Element): Scene[A] = perfed("addElement") {
    elements.get(e.id) match {
      case Some(_) => this

      case _ =>
        onElement(
          e => elementAdded(e),
          (e, touching) => {
            elementRemoved(e)
            elementRemoved(touching)
          }) (e)
        copy(elements = elements + ((e.id, e)))
    }
  }
  
  private def removeElement(e: Element): Scene[A] = perfed("removeElement") {
    elements.get(e.id) match {
      case Some(_) =>
        onElement(
          e => elementRemoved(e),
          (e, touching) => {
            for ( t <- elements.get(e.touching) ) elementAdded(t)
            viewers foreach (_ ! VisibleRemoved(e) )
            physics.removeWorldObject(e)
          }) (e)
        copy(elements = elements - e.id)

      case _ => this
    }
  }
  
  private def elementAdded(e: Element) {
    viewers foreach (_ ! VisibleAdded(e) )
    physics.addWorldObject(e)
  }
  
  private def elementRemoved(e: Element) {
    viewers foreach (_ ! VisibleRemoved(e) )
    physics.removeWorldObject(e)
  }
  
  private def onElement[T](whenNotTouching: Element => T, whenTouching: (Element, Element) => T)(e: Element): T = {
    if ( hideTouching ) {
      val touching = elements.get(e.touching)
      // hidden faces are removed from the active scene
      touching match {
        case None    =>
          whenNotTouching(e)

        case Some(t) =>
          whenTouching(e, t)
      }
    } else {
      whenNotTouching(e)
    }
  }
  
  def onElements[T](whenNotTouching: Element => T, whenTouching: (Element, Element) => T): Map[ID, T] = {
    elements map { case (id, e) =>
      (id, onElement(whenNotTouching, whenTouching)(e))
    }
  }
  
  def getElement(id: ID) = elements.get(id)
}