
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps

class Rational(val a: Int, val b: Int) {
  def this(a: Int) = this(a, 1)
  def this() = this(1,1) //Choix arbitraire

  override def toString: String = this.a+"/"+this.b
}


















