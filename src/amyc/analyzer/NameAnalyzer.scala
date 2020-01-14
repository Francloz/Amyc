package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

import N._

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)

    //For each check  that there are no modules by the same name
    modNames.foreach { 
      case (name, modules) =>
        if (modules.size > 1) {
          fatal(s"Two modules named $name in program", modules.head.position)
        }
    }

    modNames.keys.toList foreach table.addModule
    
    // Helper method: will transform a nominal type to a symbolic type,
    // given that we are within module 'inModule'.
    def toSymbolicType(tpe : N.Type, module : String, where : Positioned) : S.Type = {
      val new_type = tpe match {
        case N.IntType => S.IntType
        case N.StringType => S.StringType
        case N.BooleanType => S.BooleanType
        case N.UnitType => S.UnitType

        case N.ClassType(QualifiedName(mod,id)) => 
          S.ClassType(table.getType(mod getOrElse module, id) match {
            case Some(x) => x
            case _ =>  fatal(s"Could not find type $id", where.position)
        })
      }
      new_type
    }

    // Step 2: Check name uniqueness of definitions in each module
    modNames.foreach { 
      case (name, modules) =>
        modules.map {_.defs}.flatten
          // Group by name
          .groupBy(_.name)
          //For each check that there are no equal arguments 
          .foreach { 
            case (name, defs) =>
              if (defs.size > 1) {
                fatal(s"Two definitions with name: $name in program", defs.head.position)
          }
        }
    }
   
    // Step 3: Discover types and add them to symbol table
    modNames.foreach { 
      case (name, modules) =>
        modules.map { x=> (x.name, x.defs) }.foreach { 
            x =>  x match {case (moduleName,defs) => defs
            // Filter abstract classes
            .filter{defs => defs match {
              case AbstractClassDef(_) => true
              case _ => false 
              }
            }
            // For each add the type
            .foreach {definition => 
              table.addType(moduleName,definition.name)
            }
          }
        }
      }
    
    // Step 4: Discover type constructors, add them to table
    modNames.foreach { 
      case (file, modules) =>
        modules.map { module => (module.name, module.defs) }.foreach { 
          pair =>  pair match {
            case (modName,defs) => defs
              // Filter the CaseClassDef's
              .filter{defs => defs match {
                case CaseClassDef(_,_,_) => true
                case _ => false 
                }
              }
              // For each add it if the parent type exists
              .foreach {caseClass => caseClass match {
                case CaseClassDef(name, fields, parent) =>
                  table.addConstructor(modName, name, 
                    fields.map{ x => toSymbolicType(x.tpe, modName, caseClass)}, 
                    // Check that the parent exists
                    table.getType(modName, parent) match {
                      case Some(x) => x
                      case _ => fatal(s"Parent class $parent not found", caseClass.position)
                    })
                }
              }
          }
        }
      }

    // Step 5: Discover functions signatures, add them to table
    modNames.foreach { 
    case (name, modules) =>
      modules.map { x=> (x.name, x.defs) }.foreach { 
          x =>  x match {case (moduleName,defs) => defs
          // Filter the functions from the definitions
          .filter{defs => defs match {
            case FunDef(_,_,_,_) => true
            case _ => false 
            }
          }
          // For each function add it to the table
          .foreach { fun => fun match {
              case FunDef(name, params, retType, _) =>
              //Check for duplicate arguments
              params.groupBy(_.name).foreach { 
                case (name, args) =>
                  if (args.size > 1) {
                    fatal(s"Duplicate argument $name found", fun.position)
                  }
              };
              table.addFunction(moduleName, name, 
                params.map{case ParamDef(_, t) => toSymbolicType(t.tpe, moduleName, fun)}, 
                toSymbolicType(retType.tpe, moduleName, fun))
            }
          }
        }
      }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions

    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones
    
    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      // Abstract classes
      case N.AbstractClassDef(name) =>
        S.AbstractClassDef(table.getType(module, name) match { 
          case Some(x) => x 
          case None => fatal(s"Abstract class $name not found", df.position)
        })
      
      // Case classes
      case N.CaseClassDef(name, _, _) =>
        val (id, params) = table.getConstructor(module, name) match {
          case Some(x) => x 
          case None => fatal(s"Constructor $name not found", df.position)
          };
        S.CaseClassDef(id, 
                       params.argTypes.map {x => S.TypeTree(x) }, 
                       params.parent)

      // Functions 
      case fd: N.FunDef =>
        transformFunDef(fd, module)

    }}.setPos(df)


    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)
      
      // Check for repeated arguments
      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      
      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match {
              case N.WildcardPattern() => (S.WildcardPattern().setPos(pat.position), List())
              
              case N.LiteralPattern(lit) => (S.LiteralPattern(lit match {
                case N.IntLiteral(value) => S.IntLiteral(value).setPos(lit.position)
                case N.BooleanLiteral(value) => S.BooleanLiteral(value).setPos(lit.position)
                case N.StringLiteral(value) => S.StringLiteral(value).setPos(lit.position)
                case N.UnitLiteral() => S.UnitLiteral().setPos(lit.position)
              }).setPos(pat.position), List())
              
              case N.IdPattern(name) => 
                // Create a new fresh id for the identifier
                val fresh_id = Identifier.fresh(name); 
                
                // Checkfor variable shadowing
                if(params.exists(x => x._1 == name))
                  warning(s"Local variable $name shadows function parameter", expr.position);
                if(locals.exists(x => x._1 == name))
                  fatal(s"Variable redefinition: $name", expr.position);

                (S.IdPattern(fresh_id).setPos(pat.position), List((name, fresh_id)))

              case N.CaseClassPattern(qname, args) => 
                
                // Get the constructor from the table
                val (id, constr) = (qname match{
                  case N.QualifiedName(mod, id) => 
                    table.getConstructor(mod getOrElse module, id) getOrElse fatal(s"Constructor ${qname.name} not found.", expr.position)
                }) 
                
                // Recursively check the arguments
                val matched_patterns = args.map {transformPattern(_)};
                val matched_args = matched_patterns.map {_._1};

                // Check that the number of arguments match
                if(matched_args.length != constr.argTypes.length)
                  fatal("Unmatching number of arguments", pat.position)
                
                // Check name duplicates
                val flattened_map = matched_patterns.map {_._2}.flatten;
                
                // Check for duplicates
                (flattened_map).groupBy(_._1).foreach(x => if(x._2.length > 1) fatal(s"Duplicate of ${x._1} found in match case.", pat.position));
                
                // Return the pattern
                (S.CaseClassPattern(id, matched_args).setPos(pat.position), flattened_map)
            }
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            S.MatchCase(newPat, transformExpr(rhs)(module, (params, locals ++ moreLocals.toMap)))
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

        // Variables
        case N.Variable(name) => 
          if(!locals.exists(_._1 == name) && !params.exists(_._1 == name))
            fatal(s"Variable $name not found", expr.position)

          val id = if (!locals.exists(_._1 == name)) params(name) else locals(name)
          S.Variable(id)

        // Literals
        case N.IntLiteral(value) => S.IntLiteral(value)
        case N.BooleanLiteral(value) => S.BooleanLiteral(value)
        case N.StringLiteral(value) => S.StringLiteral(value)
        case N.UnitLiteral() => S.UnitLiteral()
      
        // Binary operators
        case N.Plus(lhs, rhs) =>       S.Plus(transformExpr(lhs), transformExpr(rhs))
        case N.Minus(lhs, rhs) =>      S.Minus(transformExpr(lhs), transformExpr(rhs))
        case N.Times(lhs, rhs) =>      S.Times(transformExpr(lhs), transformExpr(rhs))
        case N.Div(lhs, rhs) =>        S.Div(transformExpr(lhs), transformExpr(rhs))
        case N.Mod(lhs, rhs) =>        S.Mod(transformExpr(lhs), transformExpr(rhs))
        case N.LessThan(lhs, rhs) =>   S.LessThan(transformExpr(lhs), transformExpr(rhs))
        case N.LessEquals(lhs, rhs) => S.LessEquals(transformExpr(lhs), transformExpr(rhs))
        case N.And(lhs, rhs) =>        S.And(transformExpr(lhs), transformExpr(rhs))
        case N.Or(lhs, rhs) =>         S.Or(transformExpr(lhs), transformExpr(rhs))
        case N.Equals(lhs, rhs) =>     S.Equals(transformExpr(lhs), transformExpr(rhs))
        case N.Concat(lhs, rhs) =>     S.Concat(transformExpr(lhs), transformExpr(rhs))

        // Unary operators
        case N.Not(e) => S.Not(transformExpr(e))
        case N.Neg(e) => S.Neg(transformExpr(e))
        
        // Complex expressions that do NOT modify the name mapping
        case N.Sequence(e1, e2) => S.Sequence(transformExpr(e1), transformExpr(e2))
        case N.Ite(cond, thenn, elze) => S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        case N.Error(msg) => S.Error(transformExpr(msg))
        case N.Asignation(name, value) => S.Asignation(locals(name), transformExpr(value))

        // Expressions that modify the name mapping
        case N.Let(N.ParamDef(name, tt), value, body, mutable) => 
          val fresh_id = Identifier.fresh(name);
          
          // Check if the variable already exists
          if(params.exists(x => x._1 == name))
            warning(s"Local variable $name shadows function parameter", expr.position);
          if(locals.exists(x => x._1 == name))
            fatal(s"Variable redefinition: $name", expr.position);

          // Continue analyzing and make sure you shadow the redefined parameter if necessary
          S.Let(S.ParamDef(fresh_id, S.TypeTree(toSymbolicType(tt.tpe, module, expr))), 
                transformExpr(value), 
                transformExpr(body)(module, (params.filter(_._1 != name), locals ++ Map(name -> fresh_id))),
                mutable)
        
        case N.Call(qname: QualifiedName, args: List[Expr]) => 
          // Get the funcion identifier
          val (mod, id) = qname match{
            case N.QualifiedName(mod, id) => (mod getOrElse module, id) 
          }
          // Get the function signature
          val (fun_id, fun_sig) = (table.getConstructor(mod, id) 
            getOrElse (table.getFunction(mod, id) 
              getOrElse fatal(s"${qname.name} not found", expr.position))
          )
          // Check the number of arguments
          if(args.length != fun_sig.argTypes.length)
            fatal(s"Unmatching number of arguments", expr.position)

          S.Call(fun_id, args.map(transformExpr))
        }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)
  }
}
