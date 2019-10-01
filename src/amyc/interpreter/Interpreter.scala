package amyc
package interpreter

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier
import analyzer.SymbolTable
import java.io._

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this.asInstanceOf[IntValue].i
    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b
    def asString: String = this.asInstanceOf[StringValue].s

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
    }
  }
  case class IntValue(i: Int) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case class StringValue(s: String) extends Value
  case object UnitValue extends Value
  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v

    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt")    -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString")  -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt")     -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString")   -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined
    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name
    def findFunction(owner: String, name: String) = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case function@FunDef(fn, _, _, _) if fn.name == name => function
      }.get
    }

    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      expr match {
        case Variable(name) =>
          locals(name)
        case IntLiteral(i) =>
          IntValue(i)
        case BooleanLiteral(b) =>
          BooleanValue(b)
        case StringLiteral(s) =>
          StringValue(s)
        case UnitLiteral() =>
          UnitValue
        case Plus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt + interpret(rhs).asInt)
        case Minus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt - interpret(rhs).asInt)
        case Times(lhs, rhs) =>
          IntValue(interpret(lhs).asInt * interpret(rhs).asInt)
        case Div(lhs, rhs) =>
          val divisor : Int = interpret(rhs).asInt;
          if(divisor == 0)
          ctx.reporter.fatal("Division by 0.")
          else
            IntValue(interpret(lhs).asInt / divisor)
        case Mod(lhs, rhs) =>
          val divisor : Int = interpret(rhs).asInt;
          if(divisor == 0)
            ctx.reporter.fatal("Division by 0.")
          else
            IntValue(interpret(lhs).asInt % divisor)
        case LessThan(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)
        case LessEquals(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)
        case And(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean && interpret(rhs).asBoolean)
        case Or(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean || interpret(rhs).asBoolean)
        case Equals(lhs, rhs) =>
          (interpret(lhs), interpret(rhs)) match {
            case (BooleanValue(b1),BooleanValue(b2)) => BooleanValue(b1==b2)
            case (IntValue(i1),IntValue(i2)) => BooleanValue(i1==i2)
            case (UnitValue,UnitValue) => BooleanValue(true)
            case (h,t) => BooleanValue(h eq t)
          }
        case Concat(lhs, rhs) =>
          StringValue(interpret(lhs).asString ++ interpret(rhs).asString)
        case Not(e) =>
          BooleanValue(! interpret(e).asBoolean)
        case Neg(e) =>
          IntValue(- interpret(e).asInt)
        case Call(qname, args) => 
          val  interpretedArgs = for(arg <- args) yield interpret(arg);
          if(isConstructor(qname))
          {
            CaseClassValue(qname,interpretedArgs)
          }
          else if(builtIns.exists(_._1 == (findFunctionOwner(qname),qname.name)))
          { 
            builtIns.get(findFunctionOwner(qname),qname.name).head(interpretedArgs)
          }
          else
          {
            val function = findFunction(findFunctionOwner(qname),qname.name);
            val idList = for(param <- function.params) yield param.name;
            val newLocals = (idList zip interpretedArgs).toMap;
            interpret(function.body)(newLocals)
          }
        case Sequence(e1, e2) =>
          interpret(e1); interpret(e2)
        case Let(df, value, body) =>
          interpret(body)(locals + (df.name -> interpret(value)))
        case Ite(cond, thenn, elze) =>
          if (interpret(cond).asBoolean) interpret(thenn) else interpret(elze)
        case Match(scrut, cases) =>
          val evS = interpret(scrut)
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) =>
                Some(List())
              case (_, IdPattern(name)) =>
                Some(List(name -> v))
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                if(i1 == i2) Some(List()) else None
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                if(b1 == b2) Some(List()) else None
              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                None
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                Some(List())
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                val match_list = for(x <- realArgs.zip(formalArgs)) yield matchesPattern(x._1,x._2);
                if(con1 == con2 && !match_list.exists(e => !e.isDefined)) Some(match_list.flatten.flatten) else None 
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for {
            MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          } {
            return interpret(rhs)(locals ++ moreLocals)
          }
          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")

        case Error(msg) =>
          ctx.reporter.fatal(interpret(msg).asString)
      }
    }

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpret(e)(Map())
    }
  }
}
