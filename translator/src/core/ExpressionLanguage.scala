package core
import ast._

import org.kiama.rewriting.Rewriter._
import org.kiama.rewriting.Strategy
import scala.util.parsing.combinator._
import org.kiama.attribution.Attributable
import org.kiama.attribution.Attribution._
import org.kiama.output.PrettyPrinter


object ExpressionLanguageInterpreter extends Parser with Attributes with Optimizer with ELPrettyPrinter {
  val marginSize = 45
  val useCustomPrettyPrinter = true
  
  case class Runtime(program : Program) {
  	//holds the runtime values of variables
  	private var env = Map.empty[VarDef,Int] 
 
  	/**
  	 * Evaluate a given expression
  	 */
    private def eval(exp : Exp): Int = {
      exp match {
		case Num(a)    	 => a
		case Mul(a, b) 	 => eval(a) * eval(b)
		case Add(a, b)   => eval(a) + eval(b)
		case Neg(a)    	 => -eval(a)
		case Shl(a, b) 	 => eval(a) << eval(b)
		case vr : VarRef => {
		  val varDef = vr->definition
		  //if we could resolve the definition, then it must
		  //be in the environment
		  env.get(varDef).get
		}
	  }
    }
    
  	/**
  	 * Execute the given program
  	 */
    def execute() = {
      program.stms foreach {
	    case exp : Exp => {
	  	  println(eval(exp))
	    }
	    case Assign(varRef, exp) => {
	  	  val varDef = varRef->definition
	  	  env = env + (varDef -> eval(exp))
	    }
	    case vd : VarDef => {
	      env = env + (vd -> 0)
	    }
	    case _ => 
      }
    }
  }
  
  def padToMargin(s : String) =  s.padTo(marginSize, " ").mkString
  
  def prettyPrinted(program: Program) = {
    if (useCustomPrettyPrinter) pretty(toDoc(program), marginSize) 
    else pretty(product(program), marginSize)
  }
    
      
  def printProgramsSideBySide(original: Program, optimized: Program) {
    val originalArray = prettyPrinted(original).split("\\n")
    val optimizedArray = prettyPrinted(optimized).split("\\n")
    val sizeDiff = originalArray.length - optimizedArray.length
    
    val zipped = 
      if (sizeDiff > 0)
        (originalArray zip (optimizedArray.toList ++ List.fill(sizeDiff)(" "))).toList
      else 
        ((originalArray.toList ++ List.fill(-sizeDiff)(" ")) zip optimizedArray).toList
    
    val rows = for {
      (original, optimized) <- zipped
    } yield ( padToMargin(original) + optimized)
    
    println(padToMargin("ORIGINAL:") + "OPTIMIZED:")
    rows foreach ( println(_))
  }
  
  def compareOriginalAndOptimized(original : Program) {
    initTree(original)
    val optimizedProgram = optimize(original)
  	initTree(optimizedProgram)
  	printProgramsSideBySide(original, optimizedProgram)
  	  	 
  	println(List.fill(marginSize*2)('-').mkString)
  	println("ORIGINAL:")
  	Runtime(original).execute()
  	  	 
  	println("\nOPTIMIZED:")
  	Runtime(optimizedProgram).execute()
  }
  
  
  def main(args: Array[String]) {
     require(args.size == 1, "I've got nothing to execute. Please supply me with an *.el source file!")
  	 val fileReader = new java.io.FileReader(args(0))
     
	 parse(fileReader) match {
  	   case Success(program, input)=> {
  		 compareOriginalAndOptimized(program)  
  	   }
  	   case failure => println(failure)
  	 }
  }
}
