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
    var split:Array[String] = new Array[String](2)
    split = str.split("/")
    new Rational(split(0), split(1))

  override def toInt(x: Rational): Int = ???

  override def toLong(x: Rational): Long = ???

  override def toFloat(x: Rational): Float = ???

  override def toDouble(x: Rational): Double = ???

  override def compare(x: Rational, y: Rational): Int = ???
}






