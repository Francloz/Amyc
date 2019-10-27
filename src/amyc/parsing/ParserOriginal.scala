package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion.syntactic._
import scallion.syntactic.visualization._

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

  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  lazy val module: Syntax[ModuleDef] = 
    (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr) ~ "}").map {
      case obj ~ id ~ _ ~ defs ~ body ~ _ => ModuleDef(id, defs.toList, body).setPos(obj)
    }

  //-----------------------------------------IDENTIFIERS---------------------------------------------
  
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }
  
  //-----------------------------------------DEFINITIONS---------------------------------------------
  
  lazy val definition: Syntax[ClassOrFunDef] = functionDefinition | abstractClassDefinition | caseClassDefinition
   
  lazy val functionDefinition: Syntax[ClassOrFunDef] = 
    (kw("def") ~ identifierPos ~ "(".skip ~ parameters ~ ")".skip ~ ":".skip ~  typeTree ~ "{".skip ~ expr ~ "}".skip).map {
      case kw ~ id ~ params ~ rtype ~ body => FunDef(id._1, params, rtype, body).setPos(kw)
    }
  
  lazy val abstractClassDefinition: Syntax[ClassOrFunDef]  =
    (kw("abstract") ~ kw("class") ~ identifierPos).map {
      case kw ~ _ ~ id => AbstractClassDef(id._1).setPos(kw)
    } 
  
  lazy val caseClassDefinition: Syntax[ClassOrFunDef] = 
    (kw("case") ~ kw("class") ~ identifierPos ~ "(".skip ~ parameters ~ ")".skip ~ kw("extends") ~ identifier).map {
      case kw ~ _ ~ id ~ params ~ _ ~ parent => CaseClassDef(id._1, getTypes(params), parent).setPos(kw)
    } 
  
  //--------------------------------------------PARAMETERS-------------------------------------------
  
  def getTypes(params : List[ParamDef]) : List[TypeTree] = {
    for (p <- params) yield { 
      p match {
        case ParamDef(name, t) => t
      }
    }
  } 
  
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  lazy val parameter: Syntax[ParamDef] = 
    (identifier ~ ":".skip ~ typeTree).map {
      case id ~ typ => ParamDef(id, typ) 
    }

  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  }

  lazy val identifierType: Syntax[TypeTree] = 
    (identifier ~ (".".skip ~ identifier).opt).map {
      case mod ~ Some(id : String) => TypeTree(ClassType(QualifiedName(Some(mod), id)))
      case id ~ None => TypeTree(ClassType(QualifiedName(None, id)))
    }
    
  // ---------------------------------------EXPRESSIONS----------------------------------------------
  /*
  id         
  | literal
  | UnaryOp expr
  | [id.]?id (args)
  | val ParamDef = Expr; Expr
  | if (Expr) {Expr} else {Expr}
  | error(Expr)
  | ( Expr )
  | expr; expr
  | expr BinOp expr
  | expr match {MatchCase+}

  |------------|-|------|------------------|-----|--------------|-|------|
  def x : Int = 5; x + x; def y : Boolean = False; def z : Int = 3; x + z
  */
  lazy val expr: Syntax[Expr] = recursive {(defExpr.opt ~ value ~ (";".skip ~ expr).opt).map {
    case None ~ value ~ None => value
    case Some(param) ~ value ~ Some(body) => Let(param, value, body)
    case None ~ value ~ Some(seq) => Sequence(value, seq) 
  } 
  }
  
  lazy val defExpr: Syntax[ParamDef] = (kw("val") ~ identifier ~ ":".skip ~ typeTree ~ "=".skip).map {
    case kw ~ id ~ tt => ParamDef(id, tt).setPos(kw.position) 
  } 
  
  lazy val ifExpr : Syntax[Expr] = 
    (kw("if").skip ~ "(".skip ~ expr ~")".skip ~ "{".skip ~ expr ~ "}".skip ~ kw("else").skip ~ "{".skip~ expr ~ "}".skip).map {
      case cond ~ i ~ e => Ite(cond,i,e)
    }
  lazy val value: Syntax[Expr] = ((ifExpr | binOp ) ~ manyMatches.opt).map{
    case value ~ Some(matches : List[List[MatchCase]]) => matches.foldLeft(value)((exp, list) => Match(exp, list)) 
  }

  lazy val manyMatches : Syntax[List[List[MatchCase]]]  = recursive { 
    (matchExpression ~ manyMatches.opt).map {
      case first ~ Some(following) => first :: following
      case first ~ None => List(first)
    }
  }

  lazy val matchExpression : Syntax[List[MatchCase]] =
    (kw("match").skip ~ "{".skip ~ oneOrMoreCases ~ "}".skip).map {
      case cases => cases
    }
  
  lazy val binOp : Syntax[Expr] = recursive {
    operators(factor)(
      op("*") | op("/") | op("%") is LeftAssociative,
      op("+") |  op("-") is LeftAssociative,
      op("<") |  op("<=") is LeftAssociative,
      op("&&") is LeftAssociative,
      op("||") is LeftAssociative
    ) {
      case (l, "+",  r) => Plus(l, r)
      case (l, "-",  r) => Minus(l, r)
      case (l, "/",  r) => Div(l, r)
      case (l, "%",  r) => Mod(l, r)
      case (l, "*",  r) => Times(l, r)
      case (l, "<",  r) => LessThan(l, r)
      case (l, "<=", r) => LessEquals(l, r)
      case (l, "&&", r) => And(l, r)
      case (l, "||", r) => Or(l, r)
      case (l, "++", r) => Concat(l, r)
      case (l, "==", r) => Equals(l, r)
    }
  }

  lazy val factor: Syntax[Expr] = 
    ((op("-") | op("!")).opt ~ basic).map {
      case None ~ e => e
      case Some("-") ~ e => Neg(e)
      case Some("!") ~ e => Not(e)
    }

  lazy val basic: Syntax[Expr] = 
    (("(".skip ~ expr ~ ")".skip) 
    | error 
    | ("(".skip ~ expr ~ ")".skip) 
    | literal.up[Expr]
    | variableOrCall)
  
  lazy val error: Syntax[Expr] = (kw("error") ~ "(".skip ~ value ~ ")".skip).map{
    case kw ~ message => Error(message).setPos(kw.position)
  }
  
  lazy val identModule: Syntax[QualifiedName] = 
    (identifier ~ (".".skip ~ identifier).opt).map {
      case first ~ Some(second) => QualifiedName(Some(first), second)
      case first ~ None => QualifiedName(None, first)
    }
  lazy val variableOrCall: Syntax[Expr] = 
    (identModule ~ ("(".skip ~ arguments.opt ~ ")".skip).opt).map {
      case id ~ Some(Some(params)) => Call(id, params)
      case id ~ Some(None) => Call(id, List())
      case QualifiedName(None, first) ~ None => Variable(first)
    }

  lazy val arguments: Syntax[List[Expr]] = recursive {
    (expr ~ (",".skip ~ arguments).opt).map {
      case first ~ Some(rest) => first :: rest
      case first ~ None => List(first)
    } 
  }

  lazy val oneOrMoreCases: Syntax[List[MatchCase]] = recursive{
    (kw("case") ~ pattern ~ "=>".skip ~ expr ~ oneOrMoreCases.opt).map {
      case kw ~ matched ~ value ~ Some(following) =>  MatchCase(matched, value).setPos(kw.position) :: following
      case kw ~ matched ~ value ~ None => List(MatchCase(matched, value).setPos(kw.position))
    }
  }

  // ----------------------------------------LITERALS----------------------------------------------

  lazy val literal: Syntax[Literal[_]] = endLit | unitLit
  
  lazy val unitLit : Syntax[Literal[_]] = 
    ("(".skip ~ ")").map {
      case _ => UnitLiteral() 
    }
  lazy val endLit : Syntax[Literal[_]] = accept(LiteralKind) {
    case tk@IntLitToken(value) => IntLiteral(value) 
    case tk@StringLitToken(value) => StringLiteral(value)   
    case tk@BoolLitToken(value) => BooleanLiteral(value)   
  }

  // -----------------------------------------PATTERNS-----------------------------------------------
  
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
  
  //--------------------------------------------END--------------------------------------------------
  
  // Ensures the grammar is in LL(1), otherwise prints some counterexamples
  lazy val checkLL1: Boolean = {
    // graphs.outputGraph[Expr](expr, "./", "graph");
    println(grammars.getGrammar[Expr](expr).pretty());
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
