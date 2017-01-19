package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

// populating a map
// var A:Map[Char,Int] = Map()
// A + = ('I' -> 1)
// A + = ('J' -> 5)
// A + = ('K' -> 10)
// A + = ('L' -> 100)

object Calculator {
  def computeValues(
    namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    var result = Map[String, Signal[Double]]()
    namedExpressions.foreach {
      case (variableName, operation) => result += (variableName -> Signal(eval(operation(), namedExpressions - variableName)))
      // (namedExpressions - variableName) because the same variable does not refer itself.
    }
    result
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(a)   => a
      case Plus(a, b)   => eval(a, references) + eval(b, references)
      case Minus(a, b)  => eval(a, references) - eval(b, references)
      case Times(a, b)  => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
      case Ref(name)    => eval(getReferenceExpr(name, references), references - (name))
    }
  }

  /**
   * Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
