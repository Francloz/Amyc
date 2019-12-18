/*
I believe there is some character that crashes the tokenizer. I have not found what it is yet. It seems to be a character that is not printable.
I  would  advise checking the encoding of the file if the compilation fails and remove any unusual characters.
*/

package amyc
package codegen

import analyzer._
import ast.Identifier
import ast.SymbolicTreeModule.{Call => AmyCall, Div => AmyDiv, And => AmyAnd, Or => AmyOr, _}
import utils.{Context, Pipeline}
import wasm._
import Instructions._
import Utils._

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[(Program, SymbolTable), Module] {
  

  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v
    
    // Show the message str
    def reportStr(str : String) : Code = {
      mkString(str) <:> Call("Std_printString") <:> Drop 
    }

    // Show the local i
    def reportVal(i : Int) : Code = {
      GetLocal(i) <:> reportInt
    }

    // Show the integer on the stack
    def reportInt : Code = {
      Call("Std_digitToString") <:> Call("Std_printString") <:> Drop 
    }

    //  Shows the memory state
    def report : Code  = {
      mkString("Memory ") <:> 
      Const(0)  <:> Load <:> Call("Std_digitToString") <:> Call("String_concat") <:> mkString(" ") <:> Call("String_concat") <:> 
      Const(4)  <:> Load <:> Call("Std_digitToString") <:> Call("String_concat") <:> mkString(" ") <:> Call("String_concat") <:>
      Const(8)  <:> Load <:> Call("Std_digitToString") <:> Call("String_concat") <:> mkString(" ") <:> Call("String_concat") <:>
      Const(12) <:> Load <:> Call("Std_digitToString") <:> Call("String_concat") <:> mkString(" ") <:> Call("String_concat") <:> 
      Const(16) <:> Load <:> Call("Std_digitToString") <:> Call("String_concat") <:> mkString(" ") <:> Call("String_concat") <:>
      mkString("\nLocals ") <:> Call("String_concat") <:>
      GetLocal(0) <:> Call("Std_digitToString") <:> mkString(" ") <:> Call("String_concat") <:> Call("String_concat") <:> 
      GetLocal(1) <:> Call("Std_digitToString") <:> mkString(" ") <:> Call("String_concat") <:> Call("String_concat") <:> 
      GetLocal(2) <:> Call("Std_digitToString") <:> mkString(" ") <:> Call("String_concat") <:> Call("String_concat") <:> 
      GetLocal(3) <:> Call("Std_digitToString") <:> mkString(" ") <:> Call("String_concat") <:> Call("String_concat") <:> 
      GetLocal(4) <:> Call("Std_digitToString") <:> mkString(" ") <:> Call("String_concat") <:> Call("String_concat") <:> 
      GetLocal(5) <:> Call("Std_digitToString") <:> mkString(" ") <:> Call("String_concat") <:> Call("String_concat") <:> 
      Call("Std_printString") <:> Drop  
    }

    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect { case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) =>
        cgFunction(fd, name, false)
      } ++
      // Generate code for the "main" function, which contains the module expression
      optExpr.toList.map { expr =>
        val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
        cgFunction(mainFd, name, true)
      }
    }

    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the wasm function name from a combination of
      // module and function name, since we put everything in the same wasm module.
      val name = fullName(owner, fd.name)
      Function(name, fd.params.size, isMain){ lh =>
        val locals = fd.paramNames.zipWithIndex.toMap
        val body = cgExpr(fd.body)(locals, lh)
        if (isMain) {
          body <:> Drop // Main functions do not return a value,
                        // so we need to drop the value generated by their body
        } else {
          body
        }
      }
    }

    // Generate code for an expression expr.
    // Additional arguments are a mapping from identifiers (parameters and variables) to
    // their index in the wasm local variables, and a LocalsHandler which will generate
    // fresh local slots as required.
    def cgExpr(expr: Expr)(implicit locals: Map[Identifier, Int], lh: LocalsHandler): Code = {
      expr match {
        // Variables
        case  Variable(name) => GetLocal(locals(name))
        
        // Literals
        case  IntLiteral(value)     => Const(value)
        case  BooleanLiteral(value) => if (value) Const(1) else Const(0)
        case  StringLiteral(value)  => mkString(value)
        case  UnitLiteral()         => Const(0)
    
        // Binary integer operators
        case Plus(lhs, rhs)       => cgExpr(lhs) <:> cgExpr(rhs) <:> Add
        case Minus(lhs, rhs)      => cgExpr(lhs) <:> cgExpr(rhs) <:> Sub
        case Times(lhs, rhs)      => cgExpr(lhs) <:> cgExpr(rhs) <:> Mul
        case Mod(lhs, rhs)        => cgExpr(lhs) <:> cgExpr(rhs) <:> Rem
        case LessThan(lhs, rhs)   => cgExpr(lhs) <:> cgExpr(rhs) <:> Lt_s
        case LessEquals(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Le_s
        case Equals(lhs, rhs)     => cgExpr(lhs) <:> cgExpr(rhs) <:> Eq
        case AmyDiv(lhs, rhs)     => cgExpr(lhs) <:> cgExpr(rhs) <:> Div

        // Boolean binary operators
        case AmyAnd(lhs, rhs)     => cgExpr(lhs) <:> If_i32 <:> cgExpr(rhs) <:> Else <:> Const(0) <:> End 
        case AmyOr(lhs, rhs)      => cgExpr(lhs) <:> If_i32 <:> Const(1) <:> Else <:> cgExpr(rhs) <:> End 

        // Binary string operation
        case Concat(lhs, rhs)     => cgExpr(lhs) <:> cgExpr(rhs) <:> Call("String_concat")

        // Unary operators
        case Not(e) => cgExpr(e) <:> Eqz
        case Neg(e) => Const(0) <:> cgExpr(e) <:> Sub

        // The ; operator
        case Sequence(e1, e2) => cgExpr(e1) <:> Drop <:> cgExpr(e2)
        
        // Local variable definition
        case Let(df, value, body) => 
          val ptr = lh.getFreshLocal();
          cgExpr(value) <:>  SetLocal(ptr) <:>  cgExpr(body)(locals + (df.name -> ptr), lh)
        
        // If-then-else
        case Ite(cond, thenn, elze) => 
          cgExpr(cond) <:> If_i32 <:> cgExpr(thenn) <:> Else <:> cgExpr(elze) <:> End

        // Represents a computational error; prints its message, then exits
        case Error(msg) => 
          mkString("Error: ") <:> cgExpr(msg) <:> Call("String_concat") <:>  Call("Std_printString") <:> Unreachable  
          
        // Match expression
        case Match(scrut, cases) => 

          // Loads fields from address ptr+4*i to address ptr+4*n
          def loadAndAssign(ptr : Int, i : Int, pats : List[Pattern]) : (Map[Identifier, Int], Code) = {
            pats match {
              case x :: xs => 

                // Load field and try to assign it
                val load = GetLocal(ptr) <:> Const(4*i) <:> Add <:> Load
                val (asig, assign) = attempAsignation(x)

                xs match {  
                  // More than one field remaining
                  case y :: ys =>  
                    val (restAsig, restCode) = loadAndAssign(ptr, i+1, xs)
                    (restAsig ++ asig, load <:> assign <:> restCode <:> And)
                  
                  // One field remaining
                  case y => (asig, load <:> assign)
                }
              case x => sys.error("Invalid parameter. Empty list give.")
            }
          }
          // Creates the code of a pattern matching. Stores whether the match is successful in Locals(cmpIdx)
          def attempAsignation(pat : Pattern) : (Map[Identifier, Int], Code) ={
            pat match {
              
              case WildcardPattern() => (Map[Identifier, Int](), Drop <:> Const(1))
              
              case LiteralPattern(lit) => (Map[Identifier, Int](), cgExpr(lit) <:> Eq)
              
              case IdPattern(id) => 
                val idx = lh.getFreshLocal()
                (Map[Identifier, Int](id -> idx), SetLocal(idx) <:> Const(1))
             
              case CaseClassPattern(constr, pats) =>
                table.getConstructor(constr) getOrElse sys.error("Constructor not found") match { 
                  case ConstrSig(_, parent, index) => 

                    // Save pointer and constructor index check
                    val ptr = lh.getFreshLocal()
                    val checkConstr : Code = SetLocal(ptr) <:> GetLocal(ptr) <:> Load <:> Const(index) <:> Eq

                    // No fields (Removed extra IfElse block in the example compiler)
                    if (pats.isEmpty) 
                      (Map[Identifier, Int](), checkConstr)
                    else 
                    { 
                      // Attempst to match the arguments
                      val (moreLocals, paramCheck) = loadAndAssign(ptr, 1, pats)
                      (moreLocals,  checkConstr <:> If_i32 <:> paramCheck <:> Else <:> Const(0) <:> End) 
                    }
                }
                
            }
          }
          // Creates the code of a matchcase
          def codeMatchCase(indexSaved: Int, pat : MatchCase) : Code = {
            pat match {
              case MatchCase(a, b) => 
                // Gets the scrut and performs the pattern matching
                val (moreLocals, code) = attempAsignation(a)
                val resultCode : Code = cgExpr(b)(locals  ++ moreLocals, lh)
                GetLocal(indexSaved) <:> code <:> If_i32 <:> resultCode <:> Else
            }
          }

          // Concatenate all the match expresions (With the error at the end as default case)
          def codeMatches(indexSaved: Int, cases : List[MatchCase]) : Code = {
            cases match { 
              case x :: xs => codeMatchCase(indexSaved, x) <:> codeMatches(indexSaved, xs) <:> End
              case nil => mkString("Match error!") <:> Call("Std_printString") <:> Unreachable 
            }
          }
          
          val indexSaved = lh.getFreshLocal()
          val codeMatches_ = codeMatches(indexSaved, cases)
          val codeScrut = cgExpr(scrut)
          
          codeScrut <:> SetLocal(indexSaved) <:> codeMatches_ 

        // Function/constructor call
        case AmyCall(id, args) => 
          // Evaluates the arguments and leaves them in the stack
          def concatArgs(args : List[Expr]) : Code = {
            args match {
              case x :: y :: xs =>  cgExpr(x) <:> concatArgs(y :: xs) 
              case x :: Nil => cgExpr(x) 
              case Nil => sys.error("No arguments given")
            }
          }
          // Performs n consecutive allocations
          def storeFields(ptr : Int, i : Int, args : List[Expr]) : Code = 
            args match {
              case x :: xs =>
                val store = GetLocal(ptr) <:> Const(i*4) <:> Add <:> cgExpr(x) <:> Store
                xs match {
                  case y :: ys => store <:> storeFields(ptr, i+1, xs) 
                  case y => store
                }
              case xs => sys.error("No fields given.")
            }

          val funSig = table.getFunction(id) getOrElse (table.getConstructor(id) getOrElse sys.error("Non existent call identifier"))

          funSig match {
            case FunSig(_, _, owner) =>
              // Call function
              val call =  Call(s"${owner}_${id.toString}")
              if (args.isEmpty) call else concatArgs(args) <:> call
            case ConstrSig(_, parent, index) =>
              // Get pointer that will be returned
              // Store the constructor index
              // Store the fields (Optional)
              val ptr = lh.getFreshLocal()
              if (args.isEmpty)
                 GetGlobal(memoryBoundary) <:> SetLocal(ptr) <:> // Store pointer
                 GetGlobal(memoryBoundary) <:> Const(4) <:> Add <:> SetGlobal(memoryBoundary) <:> // Increment memory boundary 
                 GetLocal(ptr) <:> Const(index) <:> Store <:> // Store constructor index
                 GetLocal(ptr)
              else {
                GetGlobal(memoryBoundary) <:> SetLocal(ptr) <:> // Set pointer
                GetGlobal(memoryBoundary) <:> Const(args.length * 4 + 4) <:> Add <:> SetGlobal(memoryBoundary)  <:>  // Increment memory boundary
                GetLocal(ptr) <:> Const(index) <:> Store <:> storeFields(ptr, 1, args) <:> GetLocal(ptr) // Store fields
              }
          }
          
      }
    }
    
    println(program.modules flatMap cgModule);

    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }
}
