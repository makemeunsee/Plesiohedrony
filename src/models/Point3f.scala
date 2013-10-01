package models

class Point3f(val x: Float, val y: Float, val z: Float) {
    def this(xd: Double, yd: Double, zd: Double) {
      this(xd.toFloat, yd.toFloat, zd.toFloat)
    }
    
    private def arithOp(op: (Float, Float) => Float)(other: Point3f) =
      new Point3f(op(x, other.x), op(y, other.y), op(z, other.z))
    
    def +(other: Point3f) = arithOp((x1, x2) => x1 + x2)(other)
    
    def -(other: Point3f) = arithOp((x1, x2) => x1 - x2)(other)
    
    def *(other: Point3f) = {
      val prod = arithOp((x1, x2) => x1 * x2)(other)
      prod.x + prod.y + prod.z
    }
    
    def square = this * this
    
    def /(div: Float) = new Point3f(x / div, y / div, z / div)

    def *(factor: Float) = new Point3f(x * factor, y * factor, z * factor)

    def normalize = {
      val norm = math.sqrt(x * x + y * y + z * z).toFloat
      new Point3f(x / norm, y / norm, z / norm)
    }
    
    override def toString = s"($x, $y, $z)"
  }