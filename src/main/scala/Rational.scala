import scala.language.postfixOps

class Rational(val a: Int, val b: Int) {
  def this(a: Int) = this(a, 1)
  def this() = this(1,1) //Choix arbtraire
}


class RationalIsFractional(x: Rational, y: Rational) extends Fractional[Rational]{

  override def div(x: Rational, y: Rational): Rational = {
    new Rational((x.a*y.b),(x.b*y.a))
  }

  override def plus(x: Rational, y: Rational): Rational =
    new Rational((x.a*y.b)+(x.b+y.a), y.b*x.b)

  override def minus(x: Rational, y: Rational): Rational =
    new Rational((x.a*y.b)-(x.b+y.a), y.b*x.b)

  override def times(x: Rational, y: Rational): Rational =
    new Rational((x.a*y.a),(x.b*y.b))

  override def negate(x: Rational): Rational =
    new Rational(-x.a, x.b)

  override def fromInt(x: Int): Rational =
    new Rational(x, 1)

  override def parseString(str: String): Option[Rational] =
    val split:Array[String] = str.split("/")
    if (split(1) == "") None else Some(new Rational(split(0).toInt, split(1).toInt))


  override def toInt(x: Rational): Int =
    (x.a/x.b)

  override def toLong(x: Rational): Long =
    x.a/x.b

  override def toFloat(x: Rational): Float =
    x.a/x.b

  override def toDouble(x: Rational): Double =
    x.a/x.b

  override def compare(x: Rational, y: Rational): Int =
    val a = x.a*y.b
    val b = y.a*x.b
    if a>b then 1 else if a == b then 0 else -1

}

enum SymbolicFunction:
  case Constant(v: RationalIsFractional)
  case Neg(s: SymbolicFunction)
  case Add(s1: SymbolicFunction, s2: SymbolicFunction)
  case Sub(s1: SymbolicFunction, s2: SymbolicFunction)
  case Mult(s1: SymbolicFunction, s2: SymbolicFunction)


  def eval(s: SymbolicFunction): RationalIsFractional = s match
    case ArithExpr.Constant(x) =>
    case ArithExpr.Neg(x) => negate(x)
    case ArithExpr.Add(x, y) => eval(x)+eval(y)
    case ArithExpr.Sub(x, y) => eval(x)-eval(y)
    case ArithExpr.Mult(x, y) => eval(x)*eval(y)









