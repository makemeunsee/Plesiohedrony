package engine

import scala.math.sqrt

object Math {

  object SQRTS {
    val SQRT_2 = sqrt(2d)
    val SQRT_2_BY_2 = (SQRT_2 / 2d).toFloat
    val SQRT_3 = sqrt(3d)
    val SQRT_6 = sqrt(6d)
    val SQRT_6_BY_3 = (SQRT_6 / 3d).toFloat
    val SQRT_3_BY_2 = (SQRT_3 / 2d).toFloat
    val SQRT_3_BY_3 = (SQRT_3 / 3d).toFloat
    val SQRT_3_BY_6 = (SQRT_3 / 6d).toFloat
  }
  
  // positive modulo
  def %+(i: Int, d: Int) = ((i % d) + d) % d
  
  def sB2usB(b: Byte): Int = {
    (b + 256) % 256
  }
  
  def usB2sB(b: Int): Byte = {
    val cut = b % 256
    if ( cut > 127 )
      (cut - 256).toByte
    else
      cut.toByte
  }
  
  // TODO move to test
  for( i <- 0 until 256 ) {
    assert(i == sB2usB(usB2sB(i)))
  }
}