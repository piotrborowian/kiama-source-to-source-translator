package core
import org.kiama.attribution.Attributable
import org.kiama.attribution.Attribution._
import org.kiama.attribution.Decorators._
import org.kiama.rewriting.Rewriter._
import scala.collection.mutable
import ast._

trait Attributes {
  
  /**
   * Every Stm node has a parameterized attribute called variableLookup, e.g:
   * a node varRef can look up its definition by varRef->variableLookup(name)
   */
  private val variableLookup: String => Stm => Option[VarDef]= {
    paramAttr {	//Kiama parameterized attribute, will be cached
      varName => { //the first String attribute is bound to this val
        
        // if this is an immediate child of the Program class, we can look up the variable 
        // definitions in the scope (i.e. preceding the statement in question)
      	case immediateProgramChild if (immediateProgramChild.parent.isInstanceOf[Program]) => {
      	  val program = immediateProgramChild.parent[Program]
      	  val scopedStatements = program.stms.takeWhile { _ != immediateProgramChild}
      	  (scopedStatements find {
          	 case VarDef(name) => name == varName
          	 case _  => false
          }).asInstanceOf[Option[VarDef]] 
      	}
      	
      	// otherwise, just ask delegate to the parent
        case anyExp => {
          anyExp.parent[Stm]->variableLookup(varName)
        }
      }
    }
  }
  
  /**
   * Every VarRef node has a synthesized attribute called definition
   * returning its corresponding VarDef
   */
  val definition: VarRef => VarDef = 
    attr { //Kiama attribute, will be cached after computed for the first time
	  varRef => {
		(varRef->variableLookup(varRef.name)) getOrElse {
		  val varName = varRef.name
		  throw new RuntimeException( s"Reference to an undifined variable $varName")
	    }	
      }
    }
  
  /**
   * Every AstNode has a synthesized attribute called enclosingProgram
   * returning a Program instance enclosing this node
   */
  private val enclosingProgram : AstNode => Program = {
    //propagate the attribute to all children of Program
    down[AstNode, Program] { 
      case program : Program => program
    }
  }
  
  /**
   * a Program has a synthesized attribute which is a set of
   * all the variable names referenced in it
   */
  private val variableReferences : Program => Set[String] = 
    attr {
	  program => {
	    val variableReferences = mutable.Set.empty[String]
        everywheretd ( 
          query {
            case VarRef(name) => variableReferences += name
          }
        ) (program)
        variableReferences.toSet //convert into an immutable set
      }
    }
 
  /**
   * Every VarDef has a a synthesized attribute returning whether
   * a definition is referenced anywhere in the enclosing program
   */
  val isReferenced : VarDef => Boolean =
    attr {
	  varDef => {
	    val program = varDef->enclosingProgram
	    val varReferences = program->variableReferences
	    varReferences.contains(varDef.name)
	  }
  	}
  
}