package core
import scala.util.parsing.combinator._
import java.io.FileReader
import ast._

/**
 * Parser for the Expression Language
 */
trait Parser extends JavaTokenParsers {
    
    /**
     * Returns a ParseResult of the parsed Program
     */
    def parse(input : java.io.FileReader) : ParseResult[Program] = parseAll(program, input)
      
	private def program = rep((assign | varDef | exp) <~ ";") ^^ {
	  stms => Program(stms)
	}
	
	private def assign = (varRef <~ "=") ~ exp ^^ {
	  case varRef~exp => Assign(varRef, exp)
	}
	
	private def varDef = "var" ~> ident ^^ { 
	  varName => VarDef(varName) 
	}
	
	private def exp = term ~ rep("+"~>term) ^^ {
	  case x~xs => xs.fold(x)(Add(_,_))
	}
	
	private def term = factor ~ rep("*"~>factor) ^^ {
	  case x~xs => xs.fold(x)(Mul(_,_))
	}
	
	private def factor : Parser[Exp] = (
	    "-"~> factor ^^ Neg
	  | atom 
	)
	
	private def atom = "("~> exp <~")" | varRef | num
	private def varRef = ident ^^ { VarRef(_) }
	private def num = wholeNumber ^^ { n => Num(n.toInt)}
      
}