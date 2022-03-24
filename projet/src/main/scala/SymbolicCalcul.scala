

object SymbolicCalcul
  enum SymbolicFunction:
    case Constant(v: Rational)
    case Neg(s: SymbolicFunction)
    case Add(s1: SymbolicFunction, s2: SymbolicFunction)
    case Sub(s1: SymbolicFunction, s2: SymbolicFunction)
    case Mult(s1: SymbolicFunction, s2: SymbolicFunction)

  def eval(s: SymbolicFunction): Rational = s match
    case SymbolicFunction.Constant(x) => x
    case SymbolicFunction.Neg(x) => -eval(x)
    case SymbolicFunction.Add(x, y) => eval(x)+eval(y)
    case SymbolicFunction.Sub(x, y) => eval(x)-eval(y)
    case SymbolicFunction.Mult(x, y) => eval(x)*eval(y)

  @main def main(): Unit =

    val n1 = new Rational(2)
    val n2 = new Rational(3)
    val n3 = new Rational(1)
    val n4 = new Rational(4)

    val s1 = SymbolicFunction.Constant(n1)
    val s2 = SymbolicFunction.Constant(n2)
    val s3 = SymbolicFunction.Constant(n3)
    val s4 = SymbolicFunction.Add(s1, s2)
    val s5 = SymbolicFunction.Sub(s4, s3)
    println(eval(s5))

