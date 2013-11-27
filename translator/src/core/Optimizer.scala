package core
import org.kiama.rewriting.Rewriter._
import org.kiama.rewriting.Strategy
import org.kiama.==>
import ast._

/**
 * Optimizer can optimize Expression Language programs
 * so that they are beautiful and blazingly fast
 */
trait Optimizer extends Attributes {
   
  /**
   * Turns -(-n) into n
   */
  val simplifyNeg = rule {
    case Neg(Neg(x)) => x
  }
  
  /**
   * Turns addition of two same numbers into multiplication by 2
   */
  val addToMul = rule {
    case Add(x, y) if x == y => Mul(Num(2), x)
  }
  
  /**
   * Turns a multiplication by a power of two into a shift left node
   * E.g., an expression 7 * 8 is gradually rewritten like this:
   * x * 8 => x << 1 * 4 =>  x << 2 * 2=> x << 3 * 1 << x << 3 
   */
  val mulToShiftRule = {
    
    def isPowerOf2(n : Int) : Boolean =
      (n % 2 == 0) && (n == 2 || isPowerOf2(n/2)) 
     
    def mulToShift : Any ==> Option[Any] = {
      
      case Mul(Shl(x, Num(n)), Num(y)) if isPowerOf2(y)
        => mulToShift(Mul(Shl(x, Num(n+1)), Num(y/2))) //2: keep incrementing shifts until we reach the base case 
        
	  case Mul(x, Num(y)) if isPowerOf2(y)
	    => mulToShift(Mul(Shl(x, Num(1)), Num(y/2)))  //1: generate first shift
	  
	  case Mul(Num(x), y) => mulToShift(Mul(y, Num(x)))  //reverse the arguments
	  
	  case Mul(x, Num(1)) => Some(x) //base case: multiplying by x*1 yields x
	  
	  case e @ Mul(_,_)  => None //if we don't have anything interesting to do, fail the strategy
	}
    
    strategy (mulToShift) //turns mulToShift into a strategy
  }
  
  /**
   * Turn variable definitions not referenced in the program into Empty statements
   */
  val unusedVarDefsToEmpty = rule {
    case varDef : VarDef if !(varDef->isReferenced) => Empty      
  }
  
  /**
   * Eliminate empty statements
   */
  val eliminateEmpties = rule {
    case Empty::xs => xs
  }
  
  /**
   * Rewrite strategies to be used in optimizing Expression Language programs
   * All of the atomic strategies composing rewriteStrategies are disjoint, therefore
   * we use the '+' combinator which is a nondeteministic choice operator  
   */
  val rewriteStrategies : Strategy = addToMul + mulToShiftRule + simplifyNeg + unusedVarDefsToEmpty + eliminateEmpties
  
  /**
   * Optimizes the program applying the specified rewrite strategies
   * until there is nothing more to rewrite
   */
  def optimize(program : Program) : Program = rewrite {
    //keep applying the rules until there is nothing to rewrite
    reduce(rewriteStrategies)
  } (program)
  
}