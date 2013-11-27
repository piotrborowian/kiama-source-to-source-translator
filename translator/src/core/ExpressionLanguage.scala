package core
import ast._

import org.kiama.rewriting.Rewriter._
import org.kiama.rewriting.Strategy
import scala.util.parsing.combinator._
import org.kiama.attribution.Attributable
import org.kiama.attribution.Attribution._
import org.kiama.output.PrettyPrinter
import java.io.FileReader

trait ExpressionLanguageInterpreter extends Parser with Attributes with Optimizer with ELPrettyPrinter {
  val marginSize = 30
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
  	 * Execute the given program and returns the output list
  	 */
    def execute() : List[Integer] = {
      program.stms.foldLeft (List.empty[Integer]) {
	    case (acc, exp : Exp) => {
	  	  eval(exp)::acc
	    }
	    case (acc, Assign(varRef, exp)) => {
	  	  val varDef = varRef->definition
	  	  env = env + (varDef -> eval(exp))
	  	  acc
	    }
	    case (acc, vd : VarDef) => {
	      env = env + (vd -> 0)
	      acc
	    }
	    case (acc, _) => acc
      }
    } reverse
  }
  
  def padToMargin(s : String) =  s.padTo(marginSize, " ").mkString
  
  def prettyPrinted(program: Program) = {
    if (useCustomPrettyPrinter) pretty(toDoc(program), marginSize) 
    else pretty(product(program), marginSize)
  }
  
  def printSideBySide(left : List[String], right : List[String]){
    val sizeDiff = left.length - right.length
    
    val zipped = 
      if (sizeDiff > 0)
        left zip (right.toList ++ List.fill(sizeDiff)(" "))
      else 
        (left.toList ++ List.fill(-sizeDiff)(" ")) zip right
    
    val rows = for {
      (original, optimized) <- zipped
    } yield ( padToMargin(original) + optimized)
    
    rows foreach ( println(_))
  }
      
  def printSideBySide(original: Program, optimized: Program) {
    val originalArray = prettyPrinted(original).split("\\n")
    val optimizedArray = prettyPrinted(optimized).split("\\n")
    printSideBySide(originalArray.toList, optimizedArray.toList)
  }
  
  def printSideBySide(left : String, right : String) {
    println(padToMargin(left) + right)
  }
  
  def compareOriginalAndOptimized(originalProgram : Program) {
    initTree(originalProgram)
    val optimizedProgram = optimize(originalProgram)
  	initTree(optimizedProgram)
  	printSideBySide("ORIGINAL:", "OPTIMIZED")
  	printSideBySide(originalProgram, optimizedProgram)
    println(List.fill(marginSize*2)('-').mkString)
  	
    val originalOutput = Runtime(originalProgram).execute()
    val optimzedOutput = Runtime(optimizedProgram).execute()
  	printSideBySide("OUTPUT:", "OUTPUT")
    printSideBySide(originalOutput map (_.toString), optimzedOutput map (_.toString))
  }
}

object InterpreterTest extends ExpressionLanguageInterpreter {  
  def main(args: Array[String]) {
     require(args.size == 1, "I've got nothing to execute. Please supply me with an *.el source file!")
  	 val fileReader = new FileReader(args(0))
     
	 parse(fileReader) match {
  	   case Success(program, input)=> {
  		 compareOriginalAndOptimized(program)  
  	   }
  	   case failure => println(failure)
  	 }
  }
}
