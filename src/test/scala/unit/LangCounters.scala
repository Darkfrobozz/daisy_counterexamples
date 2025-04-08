package unit
import daisy.lang.Trees._
import daisy.lang.TreeOps
import daisy.lang.Constructors
import org.scalatest.funsuite.AnyFunSuite


/**
  Regression test for the basic absolute error and range computations.
*/
class LangCounters extends AnyFunSuite {

  def transform2(exp : Expr) : Option[Expr] = {
    val b = IntegerLiteral(BigInt(4))
    val c = IntegerLiteral(BigInt(3))
    val z = IntegerLiteral(BigInt(1))
    val d = IntegerLiteral(BigInt(8))
    val e = IntegerLiteral(BigInt(5))
    exp match {
      case Minus(b1, c1) if (b == b1 && c == c1) => Some(z)
      case Minus(e1, c1) if (e == e1 && c == c1) => Some(d)
      case b1 if (b == b1) => Some(e)
      case _ => None
    } 
  }

  // Gets completed reduction
  // Should get Plus(a, Minus(d, c)) with recursive = false
  /**
  * 
   *   {{{
   *     Add(a, Minus(b, c)) with replacements: Minus(b,c) -> z, Minus(e,c) -> d, b -> e
   *   }}}
   *   will yield:
   *   {{{
   *     Add(a, Minus(e, c))
   *   }}}
  */
  // Actually gets Plus(a, b)
  test("Testing  PostMap example") {
    val a = IntegerLiteral(BigInt(2))
    val b = IntegerLiteral(BigInt(4))
    val c = IntegerLiteral(BigInt(3))
    val z = IntegerLiteral(BigInt(1))
    val d = IntegerLiteral(BigInt(8))
    val e = IntegerLiteral(BigInt(5))
    val k = Plus(a, Minus(b, c))
    val res1 = TreeOps.postMap(transform2, false)(k)
    assert(res1 == Plus(a, Minus(d, c)))
    // This is what happens rather than what the example says
    // assert(res1 == Plus(a, d))
  }

  // Crashes due to trying to use constructor which is not allowed for or(b) where b = Seq and b.size < 2
  // Should not crash
  test("Testing or optimization") {
    val a = BooleanLiteral(false)
    val b = BooleanLiteral(false)
    val c = BooleanLiteral(true)
    val d = BooleanLiteral(false)
    val res1 = Constructors.or(a, b, c, d)
    assert(res1 == BooleanLiteral(true))
  }

  test("Testing and optimization") {
    val a = BooleanLiteral(true)
    val b = BooleanLiteral(true)
    val c = BooleanLiteral(true)
    val d = Or(List(a, b, c, BooleanLiteral(false)))
    val e = BooleanLiteral(false)
    val res1 = Constructors.and(a, b, c, d, e)
    assert(res1 == BooleanLiteral(false))
  }
  
  // Only get Plus(a, b)
  // Should get leaves?
  test("Subexpressions from Plus") {
    val a = IntegerLiteral(BigInt(5))
    val b = IntegerLiteral(BigInt(4))
    val res1 = TreeOps.getSubExpr(Plus(a, b))
    assert(res1 == Seq(Plus(a,b), a, b))
  }

}