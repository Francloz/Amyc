package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion.syntactic._

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Syntaxes[Token, TokenKind] with Debug[Token, TokenKind]
                 with Operators {
  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[String] = accept(OperatorKind(string)) { case OperatorToken(name) => name }
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = 
    (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr) ~ "}").map {
      case obj ~ id ~ _ ~ defs ~ body ~ _ => ModuleDef(id, defs.toList, body).setPos(obj)
    }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }
  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module.<<<<<<<<<<<<<<<
  lazy val definition: Syntax[ClassOrFunDef] = functionDefinition | abstractClassDefinition | caseClassDefinition
   
  lazy val functionDefinition: Syntax[ClassOrFunDef] = 
    (kw("def") ~ identifierPos ~ "(".skip ~ parameters ~ ")".skip ~ ":".skip ~  typeTree ~ "{".skip ~ expr ~ "}".skip).map {
      case kw ~ id ~ params ~ rtype ~ body => FunDef(id._1, params, rtype, body).setPos(kw)
    }
    // FunDef(name: Name, params: List[ParamDef], retType: TypeTree, body: Expr) 
  
  lazy val abstractClassDefinition: Syntax[ClassOrFunDef]  =
    (kw("abstract") ~ kw("class") ~ identifierPos).map {
      case kw ~ _ ~ id => AbstractClassDef(id._1).setPos(kw)
    } 
  
  lazy val caseClassDefinition: Syntax[ClassOrFunDef] = 
    (kw("case") ~ kw("class") ~ identifierPos ~ "(".skip ~ parameters ~ ")".skip ~ kw("extends") ~ identifier).map {
      case kw ~ _ ~ id ~ params ~ _ ~ parent => CaseClassDef(id._1, getTypes(params), parent).setPos(kw)
    } 
  
  // CaseClassDef(name: Name, fields: List[TypeTree], parent: Name)
  
  def getTypes(params : List[ParamDef]) : List[TypeTree] = {
    for (p <- params) yield { 
      p match {
        case ParamDef(name, t) => t
      }
    }
  } 

  // A list of parameter definitions.

  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type. <<<<<<<<<
  lazy val parameter: Syntax[ParamDef] = 
    (identifier ~ ":".skip ~ identifierType).map {
      case id ~ typ => ParamDef(id, typ) 
    }

  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  }

  // A user-defined type (such as `List`). <<<<<<<<<<<<<<<<<<<<<<
  lazy val identifierType: Syntax[TypeTree] = 
    (identifier).map {
      case id => TypeTree(ClassType(QualifiedName(None, id)))
    }
    

  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence
  /*
  id
  | literal
  | expr BinOp expr
  | UnaryOp expr
  | [id.]?id (args)
  | expr; expr
  | val ParamDef = Expr; Expr
  | if (Expr) {Expr} else {Expr}
  | Expr match {MatchCase+}
  | error(Expr)
  | ( Expr )
  */
  lazy val expr: Syntax[Expr] = recursive { 
      valueExpr | sequenceExpr | defExpr
    }
  lazy val valueExpr : Syntax[Expr] = 
    simpleExpr | ifelseExpr | 
    matchExpr | binOpExpr | unaryOpExpr | 
    ("(".skip ~ valueExpr ~ ")".skip)
  
  lazy val binOpExpr : Syntax[Expr] = ???
  lazy val unaryOpExpr : Syntax[Expr] = ???
  
  lazy val sequenceExpr : Syntax[Expr] = 
    (expr ~ ";".skip ~ expr).map {
      case first ~ second => Sequence(first, second)
    }
  lazy val defExpr : Syntax[Expr] = 
    (kw("val").skip ~ parameter ~ "=".skip ~ valueExpr ~ ";".skip ~ expr).map {
      case param ~ value ~ next => Let(param, value, next)
    }
  lazy val ifelseExpr : Syntax[Expr] = 
    (kw("if").skip ~ "(".skip ~ expr ~ ")".skip ~ "{".skip ~ expr ~ "}".skip ~ kw("else").skip ~ "{".skip ~ expr ~ "}".skip).map {
      case i ~ t ~e => Ite(i,t,e)
    }
  lazy val error : Syntax[Expr] =  (kw("error").skip ~ "(".skip ~ expr ~ ")".skip).map {
    case expres => Error(expres)
  }
  lazy val matchExpr : Syntax[Expr] = 
    (expr ~ kw("match").skip ~ "{".skip ~  oneOrMoreCases ~ "}".skip).map {
      case scrut ~ cases => Match(scrut, cases)
    }
  
  lazy val nestedExpr : Syntax[Expr] =  "(".skip ~ expr ~ ")".skip
  // Cases
  lazy val oneOrMoreCases : Syntax[List[MatchCase]] = 
    (one_case ~ many_cases).map {
      case one ~ many => one :: many 
    }
  lazy val many_cases : Syntax[List[MatchCase]] = repsep(one_case, ",").map(_.toList)
  lazy val one_case : Syntax[MatchCase] = 
    (kw("case").skip ~ pattern ~ "=>".skip ~ expr).map{
      case pat ~ body => MatchCase(pat,body)
    }
  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] = literal.up[Expr] | variableOrCall

  lazy val literal: Syntax[Literal[_]] = endLit //| ("(".skip ~ ")".skip)
  
  lazy val endLit : Syntax[Literal[_]] = accept(LiteralKind) {
    case tk@IntLitToken(value) => IntLiteral(value)        // e.g. integer literal "123"
    case tk@StringLitToken(value) => StringLiteral(value)   
    case tk@BoolLitToken(value) => BooleanLiteral(value)   
  }
  lazy val variableOrCall: Syntax[Expr] = 
    (identifier ~ 
    ( ".".skip ~ identifier).opt ~ 
    ("(".skip ~ arguments ~ ")".skip).opt).map {
      case mod ~ Some(id : String) ~ Some(args : List[Expr]) => Call(QualifiedName(Option(mod),id), args)
      case id ~ None ~ Some(args : List[Expr]) => Call(QualifiedName(None, id), args)
      case id ~ None ~ None => Variable(id)
    }
  
  lazy val arguments: Syntax[List[Expr]] = repsep(expr, ",").map(_.toList)


  // A pattern as part of a mach case.
  /*
    [Id.]Id (Patterns) | Id | Literal | _
  */
  lazy val patterns: Syntax[List[Pattern]] = repsep(pattern, ",").map(_.toList)

  lazy val pattern: Syntax[Pattern] = recursive {
    literalPattern | wildPattern | idOrCaseClassPattern
  }

  lazy val idOrCaseClassPattern: Syntax[Pattern] = 
    (identifier ~ 
    (".".skip ~ identifier ).opt ~ 
    ("(".skip ~ patterns ~ ")".skip).opt).map {
      case mod ~ Some(id : String) ~ Some(ps : List[Pattern]) => CaseClassPattern(QualifiedName(Option(mod), id), ps)
      case id ~ None ~ Some(ps : List[Pattern]) => CaseClassPattern(QualifiedName(None, id), ps)
      case id ~ None ~ None => IdPattern(id)
    } 
  
  lazy val literalPattern: Syntax[Pattern] = (literal).map {
    case lit => LiteralPattern(lit)
  }

  lazy val wildPattern: Syntax[Pattern] = (kw("_")).map {
    case kw => WildcardPattern()
  }

  // Ensures the grammar is in LL(1), otherwise prints some counterexamples
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      debug(program)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    program(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}
