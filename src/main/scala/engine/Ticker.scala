package engine

import collection.mutable.ArrayBuffer

object Ticker {

  trait Tickable {
    addTickable(Tickable.this)
    def tick(t: Int)
  }
  
  val tickRate = 20 // ms

  private val listeners = new ArrayBuffer[Tickable]
  var exiting = false

  private def addTickable(t: Tickable) {
    listeners += t
  }

  def start {
    thread.start
  }

  var pausing = false

  def pause {
    pausing = true
  }

  def unpause {
    pausing = false
  }

  private val thread = new Thread("Ticker") {
    override def run() {
      var tick = 0
      while(!exiting) {
        if (!pausing) {
          tick += 1
          listeners.foreach(_.tick(tick))
        }
        Thread.sleep(tickRate)
      }
    }
  }
}
