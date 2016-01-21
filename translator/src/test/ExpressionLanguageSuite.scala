package test

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import core.ExpressionLanguageInterpreter
import org.kiama.rewriting.Strategy
import org.kiama.rewriting.Rewriter
import java.io.FileReader
import org.kiama.attribution.Attribution._

@RunWith(classOf[JUnitRunner])
class ExpressionLanguageSuite extends FunSuite with ExpressionLanguageInterpreter {
  val testSource = "src/examples/example1.el"	
  val originalProgam = parse(new FileReader(testSource)).get
  initTree(originalProgam)
  val originalProgramResult= new Runtime(originalProgam).execute 
    
  /**
   * Generates a list of all possible subsets of the given list
   */
  def allSubsets[T](list : List[T]) : List[List[T]] = list match {
    case x::xs => {
      val childSubsets = allSubsets(xs)
      for {
      	subSet <- childSubsets
      	i <- 0 until 2
      }	yield {
        if (i == 0) subSet
        else x::subSet
      }
    }
    case Nil => List(Nil)
  }
 
  val strategies = List(addToMul, mulToShiftRule, simplifyNeg, unusedVarDefsToEmpty, eliminateEmpties)
  val allStrategyCombinations = allSubsets(strategies)
  
  def combineStrategies(strategies : List[Strategy]) : Strategy = 
    strategies.foldLeft(Rewriter.fail) {
      case (acc, s) => acc + s
  	}
  
  class TestInterpreter(strategyCombination : Strategy) extends ExpressionLanguageInterpreter {
    override val rewriteStrategies = strategyCombination
  }
  
  test("All strategy combinations yield semantically equivalent programs") {
    allStrategyCombinations foreach { strategies =>
      println(strategies)
      val combined = combineStrategies(strategies)
      val interpreter = new TestInterpreter(combined)
      val optimizedProgram = interpreter.optimize(originalProgam)
      initTree(optimizedProgram)
      val optimizedProgramResult = new interpreter.Runtime(optimizedProgram).execute
      assert(optimizedProgramResult == originalProgramResult)  
    }
    
  }
  
}