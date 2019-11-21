package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        
        // Variables
        case  Variable(name)    => topLevelConstraint(env(name)) 
        
        // Literals
        case  IntLiteral(_)     => topLevelConstraint(IntType)
        case  BooleanLiteral(_) => topLevelConstraint(BooleanType)
        case  StringLiteral(_)  => topLevelConstraint(StringType)
        case  UnitLiteral()     => topLevelConstraint(UnitType)
    
        // Binary operators
        case  Plus(lhs, rhs)       => topLevelConstraint(IntType)     ++ genConstraints(lhs, IntType)     ++ genConstraints(rhs, IntType)
        case  Minus(lhs, rhs)      => topLevelConstraint(IntType)     ++ genConstraints(lhs, IntType)     ++ genConstraints(rhs, IntType)
        case  Times(lhs, rhs)      => topLevelConstraint(IntType)     ++ genConstraints(lhs, IntType)     ++ genConstraints(rhs, IntType)
        case  Div(lhs, rhs)        => topLevelConstraint(IntType)     ++ genConstraints(lhs, IntType)     ++ genConstraints(rhs, IntType)
        case  Mod(lhs, rhs)        => topLevelConstraint(IntType)     ++ genConstraints(lhs, IntType)     ++ genConstraints(rhs, IntType)
        case  LessThan(lhs, rhs)   => topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType)     ++ genConstraints(rhs, IntType)
        case  LessEquals(lhs, rhs) => topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType)     ++ genConstraints(rhs, IntType)
        case  Concat(lhs, rhs)     => topLevelConstraint(StringType)  ++ genConstraints(lhs, StringType)  ++ genConstraints(rhs, StringType)
        case  And(lhs, rhs)        => topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        case  Or(lhs, rhs)         => topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)

        // Unary operators
        case  Not(e) => topLevelConstraint(BooleanType) ++ genConstraints(e, BooleanType)
        case  Neg(e) => topLevelConstraint(IntType)     ++ genConstraints(e, IntType)

        // Function/constructor call
        case  Call(id, args) => 
            (table.getConstructor(id)  getOrElse (table.getFunction(id) getOrElse sys.error(s"${id} not found in the table."))) match {
                case sig => 
                  Constraint(sig.retType, expected, e.position) :: args.zip(sig.argTypes).flatMap{ case (e, t) => genConstraints(e, t) }.toList
                }
        // The ; operator
        case  Sequence(e1, e2) => genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, expected)
        
        // Local variable definition
        case  Let(df, value, body) => genConstraints(body, expected)(Map(df.name -> df.tt.tpe) ++ env) ++ genConstraints(value, df.tt.tpe)
        
        // If-then-else
        case  Ite(cond, thenn, elze) => genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)

        // Represents a computational error; prints its message, then exits
        case  Error(msg) => genConstraints(msg, StringType)
        
        case Equals(lhs, rhs) =>
          val tVar = TypeVariable.fresh();
          genConstraints(lhs, tVar) ++ genConstraints(rhs, tVar)
        
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            pat match {
              case WildcardPattern() =>       (List[Constraint](), Map[Identifier, Type]())
              case IdPattern(id) =>           (List(),             Map(id -> scrutExpected))
              case LiteralPattern(lit) => lit match  {
                case IntLiteral(_)     =>     (List(Constraint(IntType, scrutExpected, pat.position)),      Map[Identifier, Type]())
                case UnitLiteral()    =>      (List(Constraint(UnitType, scrutExpected, pat.position)),    Map[Identifier, Type]())
                case StringLiteral(_)  =>     (List(Constraint(StringType, scrutExpected, pat.position)),  Map[Identifier, Type]())
                case BooleanLiteral(_)  =>    (List(Constraint(BooleanType, scrutExpected, pat.position)), Map[Identifier, Type]())
              }
              case CaseClassPattern(constrId, params) => 
                val sig = table.getConstructor(constrId) getOrElse sys.error(s"Constructor $constrId has not been added to the table.");
                val subConstraints = params.zip(sig.argTypes).map { case (param, argType) => handlePattern(param, argType) } 
                val subList = subConstraints.flatMap{ case (list, _) => list }
                val subMap  = subConstraints.flatMap{ case (_, map) => map.toList }.toMap

                (Constraint(sig.retType, scrutExpected, e.position) :: subList, subMap)
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type, expected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            genConstraints(cse.expr, expected)(env ++ moreEnv) ++ patConstraints
          }

          val scrutVar = TypeVariable.fresh();
          genConstraints(scrut, scrutVar) ++ cases.flatMap(cse => handleCase(cse, scrutVar, expected))
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    def printConstraints(l : List[Constraint]) : Unit = {
        l match {
          case x :: xs => x match { case Constraint(i,j, _) => println((i, j)); printConstraints(xs) }
          case x => ()
        }
    }
    
    // Solve the given set of typing constraints and
    // call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      // Uncomment to see constraints being solved
      // printConstraints(constraints);
      // println();

      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          
          def typeError() {
            error(s"Type error: expected type $expected, found $found", pos)
          }

          (found, expected) match {

            // Decrease the index of type variables
            case (TypeVariable(i), TypeVariable(j)) => 
              if (i < j) solveConstraints(subst_*(more, i, expected))
              else solveConstraints(subst_*(more, j, found))

            // Substitute type variables for types
            case (TypeVariable(i), other) => solveConstraints(subst_*(more, i, other))
            case (other, TypeVariable(i)) => solveConstraints(subst_*(more, i, other))
          
            // Assert equality and continue
            case (_, _) => (found, expected) match {
              // Assert primitive type equality
              case (IntType, other)         => other match {case IntType =>     ()   case _ => typeError()}
              case (BooleanType, other)     => other match {case BooleanType => ()   case _ => typeError()}
              case (StringType, other)      => other match {case StringType =>  ()   case _ => typeError()}
              case (UnitType, other)        => other match {case UnitType =>    ()   case _ => typeError()}
              
              // Assert class types equality
              case (ClassType(id), other)   => other match {
                case ClassType(idx) =>    
                  if (id != idx) typeError()
                case _ => 
                  typeError()
              }

              // Not implemented types
              case _ => sys.error(s"Constraint solver does not support types ($found, $expected).")
            }; 
            // Continue
            solveConstraints(more)
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap

        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => { solveConstraints(genConstraints(e, tv)(Map())) })
    }

    v

  }
}
