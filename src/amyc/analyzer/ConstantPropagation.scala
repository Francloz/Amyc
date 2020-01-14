/**********************************

This is where constant propagation takes place.

We move through the AST trying to remember the values of each variable, and inlining
thm whenever it is possible. This leads to the problem of multiple possible branches 
of execution and when to update thee values of mutable variables. I.e.,

val c = f();
var x = 3;
x = 4; // Ignorable
x = 5; // Ignorable

if (x + c > 0) { // Can be inlined
  x = 7; // Not ignorable. We must update the value.
}else{
  3
}

Std.printInt(x) // Do we know the values of x? Can it be inlined? 


We do this by keeping track of whether we are in an optional braanch of execution or not, 
and whether we know the value of the variables.  Explicitly expressed by the parameters:

blockedSubs := we can inline variables
forceAsig := force variable updates in-code
env(identifier) := Option[Expr] // Some(Expresion) if we know the value or None if we do not. 

To keep track of this things, the expression propagation function returns the updates to 
variales done in the expression.

I opted to not check if in every possible bbranch of execution, the values are the same. Instead,
all updates done in optional branches update the envronment and set the value of those variables 
as unknown. If the value of a variable is updated in every branch to the same value, i.e.

if (x > 0) {
  x = 1
}else{
  x = 1
}

the programmer does not deserve to have his code optimized. It is the work of an awful coder.
So insteaad of increasing code complexity by adding pairwise update comparisons, I left that 
aside to focus on other things.

The other problem I found was when to completely inline variables and remove the variable 
definition from the code. And when to inline literals without messing wwith equality by 
reference of classes and strings. For this I used 'blockedSubs' to not erase equality by  
reference in the following case:

val x : String = "";
var y : String = "";
y = x; // We know x = "", but we should not simplify it to y = "";

So we block the inlining till we find a compelling reason to allow it again, such as:

val x : String = "";
var y : String = "";
y = x + x; // We can inline

The only sensible other thing I had to pay attenion to was careful control of updates. 
When updates are and are  not meant to be done. Because as I have set things, asignations 
can be everywhere and behave as inlined operations that return the value.

(true || (x = x + 1 < 1)) // This should not remember the update of x, because it should 
not take place.

This is taken care of mainly in if-else simplifications and in operators and are explained 
down below.

**********************************/
package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object ConstantPropagation extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v
    
    def propDef(definition : FunDef) : FunDef = {
      definition match {
        case FunDef(name, params, retType, body) => FunDef(name, params, retType, propExpr(body, false)(
          params.map {case ParamDef(name,_) => (name,None)}.toMap
          , false)._1)
      }
    }

    def propExpr(e: Expr, blockedSubs : Boolean)(implicit env: Map[Identifier, Option[Expr]], forceAsig : Boolean): (Expr, Map[Identifier, Option[Expr]]) = {
      // Propagator of binary operations
      def propBinOp(cls : (Expr,Expr) => Expr, lhs : Expr, rhs : Expr) : (Expr, Map[Identifier, Option[Expr]]) = {
        
        val (newL, updatesL) = propExpr(lhs, false)
        val (newR, updatesR) = propExpr(rhs, false)(env ++ updatesL, forceAsig)

        val totalUpdate = updatesL ++ updatesR
        val result = (newL, newR) match {
          // Can be simplified further
          case (l : Literal[_], r : Literal[_]) => 
            (cls, newL, newR) match {
              // T x T => Bool
              case (Equals, _, _) =>
                // Extra variable check for redundant equalities 
                val isVarEq = (lhs, rhs) match {
                  case (Variable(v1),Variable(v2)) => v1 == v2
                  case _ => false 
                }
                if (isVarEq)
                  (BooleanLiteral(true), totalUpdate)
                else
                  (newL, newR) match {
                    case (UnitLiteral(), UnitLiteral())           => (BooleanLiteral(true), totalUpdate)
                    case (IntLiteral(vL), IntLiteral(vR))         => (BooleanLiteral(vL == vR), totalUpdate)
                    case (BooleanLiteral(vL), BooleanLiteral(vR)) => (BooleanLiteral(vL == vR), totalUpdate)
                    case _  => (cls(lhs, rhs), totalUpdate) 
                    
                    // We do not simplify equalities by reference of strings
                    // Inlining strings would be incorrect, and for proper 
                    // simplification we would need a graph of allequal variables
                    // val x = "";
                    // val y = x;
                    // var z = "";
                    // z = y;
                    // z == y ?
                  }
              // Int x Int => Int
              case (Plus,  IntLiteral(vL), IntLiteral(vR)) => (IntLiteral(vL + vR), totalUpdate)
              case (Minus, IntLiteral(vL), IntLiteral(vR)) => (IntLiteral(vL - vR), totalUpdate)
              case (Times, IntLiteral(vL), IntLiteral(vR)) => (IntLiteral(vL * vR), totalUpdate)
              case (Div,   IntLiteral(vL), IntLiteral(vR)) => (IntLiteral(vL / vR), totalUpdate)
              case (Mod,   IntLiteral(vL), IntLiteral(vR)) => (IntLiteral(vL % vR), totalUpdate)

              // Int x Int => Bool
              case (LessThan,   IntLiteral(vL), IntLiteral(vR)) => (BooleanLiteral(vL < vR), totalUpdate)
              case (LessEquals, IntLiteral(vL), IntLiteral(vR)) => (BooleanLiteral(vL <= vR), totalUpdate)

              // String x String => String
              case (Concat, StringLiteral(vL), StringLiteral(vR)) => (StringLiteral(vL ++ vR), totalUpdate)

              // Bool x Bool => Bool
              case (And, BooleanLiteral(vL), BooleanLiteral(vR)) => (BooleanLiteral(vL || vR), totalUpdate) 
              case (Or,  BooleanLiteral(vL), BooleanLiteral(vR)) => (BooleanLiteral(vL && vR), totalUpdate) 
              case (Sequence,  _, vR) => (vR, totalUpdate)    

              case _  => sys.error(s"Invalid literals found in operation ${cls(newL, newR)}")
            }
          // We know the left side is a constant
          case (vL : Literal[_], vR) => (cls, vL) match {
            case (And, BooleanLiteral(true))  => (vR, updatesL ++ updatesR)
            case (Or,  BooleanLiteral(false)) => (vR, updatesL ++ updatesR)
            case (Sequence,  _)               => (vR, updatesL ++ updatesR)
            case (And, BooleanLiteral(false)) => (BooleanLiteral(false), updatesL) 
            case (Or,  BooleanLiteral(true))  => (BooleanLiteral(true), updatesL)
            case _ => (cls(newL, newR), totalUpdate)
          }
          // We know the right side is a constant
          case (vL, vR  : Literal[_]) => (cls, vR) match {
            case (Times, IntLiteral(1)) => (vL, updatesL ++ updatesR)
            case (Div, IntLiteral(1))   => (vL, updatesL ++ updatesR)
            case (Mod, IntLiteral(1))   => (vL, updatesL ++ updatesR)
            case _ => (cls(newL, newR), totalUpdate)
          }
          // We cannot simplify further
          case _ => (cls(newL, newR), totalUpdate)
        } 
        result
      }

      val result : (Expr, Map[Identifier,Option[Expr]])= e match {
        
        // Variable
        case Variable(name) => 
          // If we know the value and the substitution is not blocked, we inline it.
          val newExpr = if (blockedSubs) e else (env(name)).getOrElse(e)
          (newExpr,Map())
        
        // Literals
        case IntLiteral(_)     => (e,Map())
        case BooleanLiteral(_) => (e,Map())
        case StringLiteral(_)  => (e,Map())
        case UnitLiteral()     => (e,Map())
    
        // Binary operators
        case Plus(lhs, rhs)       => propBinOp(Plus, lhs, rhs)
        case Minus(lhs, rhs)      => propBinOp(Minus, lhs, rhs)
        case Times(lhs, rhs)      => propBinOp(Times, lhs, rhs)
        case Div(lhs, rhs)        => propBinOp(Div, lhs, rhs)
        case Mod(lhs, rhs)        => propBinOp(Mod, lhs, rhs)
        case LessThan(lhs, rhs)   => propBinOp(LessThan, lhs, rhs)
        case LessEquals(lhs, rhs) => propBinOp(LessEquals, lhs, rhs)
        case Concat(lhs, rhs)     => propBinOp(Concat, lhs, rhs)
        case And(lhs, rhs)        => propBinOp(And, lhs, rhs)
        case Or(lhs, rhs)         => propBinOp(Or, lhs, rhs)
        case Equals(lhs, rhs)     => propBinOp(Equals, lhs, rhs)
        // The ; operator
        case Sequence(e1, e2)     => propBinOp(Sequence, e1, e2)

        // Unary operators
        case Not(e) => 
          val (newE,updates) = propExpr(e, false)
          newE match {
            case BooleanLiteral(b) => (BooleanLiteral(!b),updates)
            case _ => (Not(newE),updates)
          }
        case Neg(e) => 
          val (newE,updates) = propExpr(e, false)
          newE match {
            case IntLiteral(i) => (IntLiteral(-i),updates)
            case _ => (Neg(newE), updates)
          }
        // Function/constructor call. Doesn't inline functions as discussed in the project presentation.
        case Call(id, args) => 
          val propPairs = args.map(a => propExpr(a, false))
          val propedExpr = propPairs.map{case (a,b) => a}
          val propUpdates = if (propPairs.isEmpty) Map[Identifier, Option[Expr]]() else propPairs.map{case (a,b) => b}.reduce(_ ++ _)
          (Call(id, propedExpr), propUpdates)
        
        // Asignation of variables. If we are in a conditional branch of execution we force the asignation
        case Asignation(vName, expr) =>
          val (propedExpr, updates) = propExpr(expr, blockedSubs)
          val resultExpr = if (forceAsig || env(vName) == None) Asignation(vName, propedExpr) else propedExpr
          (resultExpr, updates + (vName -> Some(propedExpr))) 
 
        // Local variable definition
        case Let(ParamDef(vName, tt), value, body, mutable) => 
          
          val canInline : Boolean = tt.tpe match {
            case StringType => false
            case ClassType(_) => false
            case _ => true
          }
          // If it is an object or a variable, we leave the definition, if not weonly inline it.
          if (canInline && !mutable)
            {
              val (newValue, valUpdates) = propExpr(value, false)
              val newEnv = env ++ valUpdates
              val (newBody, bodyUpdates) = propExpr(body, false)(newEnv + (vName -> Some(newValue)), forceAsig)
              (newBody,  Map(vName -> Some(newValue)) ++ bodyUpdates)
            }
          else
            {
              val (newValue, valUpdates) = propExpr(value, !mutable)
              val update =  (if (canInline) (vName -> Some(newValue)) else (vName -> None))
              val bodyEnv = env ++ valUpdates + update
              val (newBody, bodyUpdates) = propExpr(body, false)(bodyEnv, forceAsig)
              (
                Let(ParamDef(vName, tt), newValue, newBody, mutable), 
                Map(update) ++ bodyUpdates
              )
            }

        // If-then-else
        case Ite(cond, thenn, elze) => 
          val (propCond, updatesCond) = propExpr(cond, false)
          
          propCond match {
            // If we know the value of the condition
            case BooleanLiteral(b) => 
              val (propThen, updatesThen) = propExpr(thenn, blockedSubs)(env ++ updatesCond, forceAsig)
              val (propElse, updatesElse) = propExpr(elze, blockedSubs)(env ++ updatesCond, forceAsig)
              if (b) (propThen, updatesCond ++ updatesThen) else (propElse, updatesCond ++ updatesElse)
            // If we do not know the value of the condition
            case _ => 
              // We propagate the branches forcing variable asignations
              val (propThen, updatesThen) = propExpr(thenn, blockedSubs)(env ++ updatesCond, true)
              val (propElse, updatesElse) = propExpr(elze, blockedSubs)(env ++ updatesCond, true)
              // Wemark any variables updated on the branchs as unknown
              val nullifiedUpdates = (env ++ updatesCond) ++ (updatesElse ++ updatesThen).map{case (a,b) => (a, None)}
              // Small optimization of negations
              propCond match {
                case Not(miniCond) => (Ite(miniCond, propElse, propThen), nullifiedUpdates)
                case _             => (Ite(propCond, propThen, propElse), nullifiedUpdates)
              }
          }

        // Error
        case Error(msg) => 
          val (newExpr, updates) = propExpr(msg, false)
          (Error(newExpr), updates)

        // Match. Doesnt not perform pattern matching as discussed in the project presentation.
        case Match(scrut, cases) => 
          // Propagate the constants on every matchcase, forcing variable asignations
          val updCases : List[(MatchCase, Map[Identifier, Option[Expr]])] = cases.map{
            case MatchCase(pat, expr) => 
              val (propedExpr, updates) = propExpr(expr, blockedSubs)(env, true)
              (MatchCase(pat, propedExpr), updates)
          }
          val (propedScrut, updatesScrut) = propExpr(scrut, false)
          // Because we do not know which branch f execution will be taken, we mark the values updated in any branch as unknown
          val nullifiedUpdates = updCases.map{case (a,b) => b}.reduce(_ ++ _).map{case (a,b) => (a, None)}
          (Match(propedScrut, updCases.map{case (a,b) => a}), nullifiedUpdates)
      }

      println(s"$e => ${result._1}\n")
      result
    }

    val newProgram = Program(
      program.modules map { case mod@ModuleDef(name, defs, optExpr) =>
        ModuleDef(
          name,
          defs map (_ match {
            case f : FunDef => propDef(f)
            case default => default  
          }),
          optExpr map (propExpr(_, false)(Map(), false)._1)
        ).setPos(mod)
      }
    ).setPos(program)

    (newProgram, table)

  }
}
