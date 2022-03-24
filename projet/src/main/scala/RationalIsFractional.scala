
class RationalIsFractional(val x: Rational, val y: Rational) extends Fractional[Rational]{

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