package core
import org.kiama.attribution.Attributable
import org.kiama.output.PrettyPrinter
import org.kiama.output._

object ast {
  sealed trait AstNode extends Attributable
  sealed trait Stm extends AstNode
  sealed trait Exp extends Stm with PrettyExpression
  
  abstract class UnaryOp(override val op :String, override val priority : Int) extends Exp with PrettyUnaryExpression {
    val fixity = Prefix
  }
  abstract class BinaryOp(override val op :String, override val priority : Int) extends Exp with PrettyBinaryExpression {
    val fixity = Infix(LeftAssoc)
  }
  
  //Expressions
  case class Num(a : Int) extends Exp
  case class VarRef(name : String) extends Exp
  case class Neg(exp : Exp) extends UnaryOp("-", 1) 
  case class Mul(left : Exp, right : Exp) extends BinaryOp("*", 2)
  case class Add(left : Exp, right: Exp) extends BinaryOp("+", 3)
  case class Shl(left : Exp, right : Exp) extends BinaryOp("<<", 4) 
  

  //Other statements
  case class VarDef(name : String) extends Stm
  case class Assign(varRef : VarRef, exp : Exp) extends Stm
  case class Program(stms: List[Stm]) extends AstNode

  //Internal nodes used for optimization
  case object Empty extends Stm
}