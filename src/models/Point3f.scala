package models

// a 3D point / vector
class Point3f(val x: Float, val y: Float, val z: Float) extends Tuple3(x, y, z) {
    def this(xd: Double, yd: Double, zd: Double) {
      this(xd.toFloat, yd.toFloat, zd.toFloat)
    }
    
    private def arithOp(op: (Float, Float) => Float)(other: Point3f) =
      new Point3f(op(x, other.x), op(y, other.y), op(z, other.z))
    
    def +(other: Point3f) = arithOp((x1, x2) => x1 + x2)(other)
    
    def -(other: Point3f) = arithOp((x1, x2) => x1 - x2)(other)
    
    def unary_- = new Point3f(-x, -y, -z)
    
    // scalar product
    def *(other: Point3f) = {
      val prod = arithOp((x1, x2) => x1 * x2)(other)
      prod.x + prod.y + prod.z
    }
    
    // cross product
    def ^(other: Point3f) = {
      new Point3f(y*other.z - z*other.y, z*other.x - x*other.z, x*other.y - y*other.x)
    }
    
    def square = this * this
    
    def /(div: Float) = new Point3f(x / div, y / div, z / div)

    def *(factor: Float) = new Point3f(x * factor, y * factor, z * factor)

    def normalize = {
      val n = norm.toFloat
      new Point3f(x / n, y / n, z / n)
    }
    
    def norm = math.sqrt(this * this)
    
    def round(digits: Int) = {
      assert (digits > -1)
      val mult = math.pow(10, digits)
      def round(f: Float) = math.round(f * mult) / mult
      new Point3f(round(x), round(y), round(z))
    }
    
    override def toString = s"($x, $y, $z)"
  }