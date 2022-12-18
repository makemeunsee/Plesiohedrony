package engine

import collection.mutable.ArrayBuffer

object Ticker {

  trait Tickable {
    addTickable(Tickable.this): Unit
    def tick(t: Int): Unit
  }

  val tickRate = 20 // ms

  private val listeners = new ArrayBuffer[Tickable]
  var exiting = false

  private def addTickable(t: Tickable): Unit = {
    listeners += t
  }

  def start(): Unit = {
    thread.start
  }

  var pausing = false

  def pause(): Unit = {
    pausing = true
  }

  def unpause(): Unit = {
    pausing = false
  }

  private val thread = new Thread("Ticker") {
    override def run(): Unit = {
      var tick = 0
      while (!exiting) {
        if (!pausing) {
          tick += 1
          listeners.foreach(_.tick(tick))
        }
        Thread.sleep(tickRate)
      }
    }
  }
}
