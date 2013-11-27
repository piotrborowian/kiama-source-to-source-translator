package core
import org.kiama.attribution.Attributable
import org.kiama.output.PrettyPrinter

object ast {
  sealed trait AstNode extends Attributable
  sealed trait Stm extends AstNode
  sealed trait Exp extends Stm

  //Expressions
  case class Num(a : Int) extends Exp
  case class Add(a : Exp, b: Exp) extends Exp
  case class Mul(a : Exp, b : Exp) extends Exp
  case class Shl(a : Exp, n : Int) extends Exp
  case class Neg(a : Exp) extends Exp
  case class VarRef(name : String) extends Exp

  //Other statements
  case class VarDef(name : String) extends Stm
  case class Assign(varRef : VarRef, exp : Exp) extends Stm
  case class Program(stms: List[Stm]) extends AstNode

  //Internal nodes used for optimization
  case object Empty extends AstNode
}