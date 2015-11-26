package perf

import scala.collection.mutable.HashMap

object Perf {
  
  val calls = new HashMap[String, (Int, Long)]
  
  def perfed[R](name: String = "")(f: => R) = {
    val t0 = System.nanoTime
    val res = f
    val lapse = (System.nanoTime - t0) / 1000 // save microseconds
    if(name != "") {
      calls.get(name) match {
        case Some((c, l)) => calls += ((name, (c+1, l+lapse)))
        case _ => calls += ((name, (1, lapse)))
      }
    } else {
      println(lapse)
    }
    res
  }
  
  def printResults() {
    calls.foreach { case (name, (count, timeSpent)) =>
      println(s"Called $name")
      println(s"\t$count times")
      println(s"\tspending ${timeSpent/1000f} ms")
      println(s"\taverage: ${timeSpent/1000f/count} ms/call")
    }
  }
}