package core
import org.kiama.output.PrettyPrinter
import org.kiama.output.PrettyExpression
import org.kiama.output.ParenPrettyPrinter
import core.ast._

trait ELPrettyPrinter extends PrettyPrinter with ParenPrettyPrinter {
   def toDoc (node : AstNode) : Doc = node match {
     case Program(stms) => ssep(stms map toDoc, ";" <> line )
     case Empty => empty
     case VarDef(name) => "var" <+> name 
     case Assign(varRef, exp) => toDoc(varRef) <+> "=" <+> toDoc(exp)
     case e : Exp => toParenDoc(e)
   }
   
   override def toParenDoc(e : PrettyExpression) = e match {
     case Num(a) => a.toString
     case VarRef(name) => name
     case e : Exp => super.toParenDoc(e)
   }

}