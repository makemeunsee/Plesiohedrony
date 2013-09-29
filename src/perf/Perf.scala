package perf

object Perf {
  
  def perfed[R](name: String = "")(f: => R) = {
    val t0 = System.currentTimeMillis
    val res = f
    println(System.currentTimeMillis - t0)
    res
  }
}