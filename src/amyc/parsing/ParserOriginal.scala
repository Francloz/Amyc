// package amyc
// package parsing

// import scala.language.implicitConversions

// import amyc.ast.NominalTreeModule._
// import amyc.utils._
// import Tokens._
// import TokenKinds._

// import scallion.syntactic._

// // The parser for Amy
// object Parser extends Pipeline[Iterator[Token], Program]
//                  with Syntaxes[Token, TokenKind] with Debug[Token, TokenKind]
//                  with Operators {
//   import Implicits._

//   override def getKind(token: Token): TokenKind = TokenKind.of(token)

//   val eof: Syntax[Token] = elem(EOFKind)
//   def op(string: String): Syntax[String] = accept(OperatorKind(string)) { case OperatorToken(name) => name }
//   def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

//   implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

//   // An entire program (the starting rule for any Amy file).
//   lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

//   // A module (i.e., a collection of definitions and an initializer expression)
//   lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr) ~ "}").map {
//     case obj ~ id ~ _ ~ defs ~ body ~ _ => ModuleDef(id, defs.toList, body).setPos(obj)
//   }

//   // An identifier.
//   val identifier: Syntax[String] = accept(IdentifierKind) {
//     case IdentifierToken(name) => name
//   }

//   // An identifier along with its position.
//   val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
//     case id@IdentifierToken(name) => (name, id.position)
//   }

//   // A definition within a module.
//   lazy val definition: Syntax[ClassOrFunDef] = functionDefinition | abstractClassDefinition | caseClassDefinition
   
//   lazy val functionDefinition: Syntax[ClassOrFunDef] = 
//     (kw("def") ~ identifierPos ~ "(".skip ~ parameters ~ ")".skip ~ ":".skip ~  typeTree ~ "=".skip ~ "{".skip ~ expr ~ "}".skip).map {
//       case kw ~ id ~ params ~ rtype ~ body => FunDef(id._1, params, rtype, body).setPos(kw)
//     }
  
//   lazy val abstractClassDefinition: Syntax[ClassOrFunDef]  =
//     (kw("abstract") ~ kw("class") ~ identifierPos).map {
//       case kw ~ _ ~ id => AbstractClassDef(id._1).setPos(kw)
//     } 
  
//     def getTypes(params : List[ParamDef]) : List[TypeTree] = {
//     for (p <- params) yield { 
//       p match {
//         case ParamDef(name, t) => t
//       }
//     }
//   } 
//   lazy val caseClassDefinition: Syntax[ClassOrFunDef] = 
//     (kw("case") ~ kw("class") ~ identifierPos ~ "(".skip ~ parameters ~ ")".skip ~ kw("extends") ~ identifier).map {
//       case kw ~ _ ~ id ~ params ~ _ ~ parent => CaseClassDef(id._1, getTypes(params), parent).setPos(kw)
//     } 

//   // A list of parameter definitions.
//   lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

//   // A parameter definition, i.e., an identifier along with the expected type.
//   lazy val parameter: Syntax[ParamDef] =  (identifier ~ ":".skip ~ typeTree).map {
//     case id~typ => ParamDef(id,typ)
//   }

//   // A type expression.
//   lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

//   // A built-in type (such as `Int`).
//   val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
//     case tk@PrimTypeToken(name) => TypeTree(name match {
//       case "Unit" => UnitType
//       case "Boolean" => BooleanType
//       case "Int" => IntType
//       case "String" => StringType
//       case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
//     }).setPos(tk)
//   }
  
//   lazy val qualifiedIdentifier: Syntax[QualifiedName] = (identifier ~ (".".skip ~ identifier).opt).map{
//     case mod ~ Some(id : String) => QualifiedName(Some(mod), id)
//     case id ~ None => QualifiedName(None, id)
//   }
//   // A user-defined type (such as `List`).
//   lazy val identifierType: Syntax[TypeTree] = qualifiedIdentifier.map {
//     case qn => TypeTree(ClassType(qn))
//   }

//   // An expression.
//   // HINT: You can use `operators` to take care of associativity and precedence
//   lazy val expr: Syntax[Expr] = recursive {
//     defExpr | seqExpr
//   }

  
//   lazy val defExpr: Syntax[Expr] = (kw("val") ~ identifier ~ ":".skip ~ typeTree ~ "=".skip ~ value ~ ";".skip ~ expr).map {
//     case kw ~ id ~ tt ~ value ~body => Let(ParamDef(id, tt), value, body).setPos(kw.position) 
//   } 
//   lazy val seqExpr: Syntax[Expr] = (value ~ (";".skip ~ expr).opt).map {
//     case value ~ None => value 
//     case value ~ Some(expr) => Sequence(value, expr) 
//   } 
//   lazy val ifExpr : Syntax[Expr] = 
//     (kw("if").skip ~ "(".skip ~ expr ~")".skip ~ "{".skip ~ expr ~ "}".skip ~ kw("else").skip ~ "{".skip~ expr ~ "}".skip).map {
//       case cond ~ i ~ e => Ite(cond,i,e)
//     }
  
//   lazy val value: Syntax[Expr] = binOpMatchOp | ifMatchOp
  

//   /* --------------------------------------------IF'S AND MATCHES--------------------------------------------------

//     This section provides ifs and matches with the posibility of being operands. 
//     The code is slightly obscure, but just adds left asociativity with binary operators 
//     to if's and matches, while preserving LL1 and match, if > binOp priority.
    
//     Some of this functionality is not present in Amy.
//   -----------------------------------------------------------------------------------------------------------------*/

//   // Turn many consecutive matches into a match expression
//   def getMatchExpr(value : Expr, matches : List[List[MatchCase]]) : Expr = {
//     matches.foldLeft(value)((expr, list) => Match(expr, list))
//   }
//   // All binary operations
//   lazy val binOperator : Syntax[String] =   op("*") | op("/") | op("%") | op("+") |  op("-") |  op("++") | op("<") |  op("<=") | op("==") | op("&&") | op("||") 

//   // binOp [match {} [op value | ? ] | ? ]  
//   lazy val binOpMatchOp: Syntax[Expr] = recursive {
//       (binOp ~ (manyMatches ~ (binOperator ~ value).opt).opt).map{
//       case value ~ Some(matches ~ None) => getMatchExpr(value, matches)
//       case value ~ None => value 
//       case value ~ Some(matches ~ Some(op ~ right)) => (getMatchExpr(value, matches): Expr, op : String,  right: Expr) match {
//         case (l, "+",  r) => Plus(l, r)
//         case (l, "-",  r) => Minus(l, r)
//         case (l, "/",  r) => Div(l, r)
//         case (l, "%",  r) => Mod(l, r)
//         case (l, "*",  r) => Times(l, r)
//         case (l, "<",  r) => LessThan(l, r)
//         case (l, "<=", r) => LessEquals(l, r)
//         case (l, "&&", r) => And(l, r)
//         case (l, "||", r) => Or(l, r)
//         case (l, "++", r) => Concat(l, r)
//         case (l, _, r) => Equals(l, r) // Wildcard to avoid warning
//       } 
//     }
//   }
//   // [! | -] ifExpr
//   // lazy val ifWithUnary : Syntax[Expr] = ((op("!") | op("-")).opt ~ ifMatchOp).map {
//   //   case None ~ value => value
//   //   case Some("!") ~ value => Not(value)
//   //   case Some(_) ~ value => Neg(value) // Wild card to avoid warning
//   // }
//   // ifExpr [match {} | ?] [ op value | ? ]
//   lazy val ifMatchOp : Syntax[Expr] = recursive {
//     (ifExpr ~ manyMatches.opt ~ (binOperator ~ value).opt).map{
//       case value ~ Some(matches) ~ None => getMatchExpr(value, matches)
//       case value ~ None ~ None => value 
//       case value ~ Some(matches) ~ Some(op ~ right) => (getMatchExpr(value, matches): Expr, op : String,  right: Expr) match {
//         case (l, "+",  r) => Plus(l, r)
//         case (l, "-",  r) => Minus(l, r)
//         case (l, "/",  r) => Div(l, r)
//         case (l, "%",  r) => Mod(l, r)
//         case (l, "*",  r) => Times(l, r)
//         case (l, "<",  r) => LessThan(l, r)
//         case (l, "<=", r) => LessEquals(l, r)
//         case (l, "&&", r) => And(l, r)
//         case (l, "||", r) => Or(l, r)
//         case (l, "++", r) => Concat(l, r)
//         case (l, _, r) => Equals(l, r) // Wildcard to avoid warning
//       } 
//       case value ~ None ~ Some(op ~ right) => (value: Expr, op : String,  right: Expr) match {
//         case (l, "+",  r) => Plus(l, r)
//         case (l, "-",  r) => Minus(l, r)
//         case (l, "/",  r) => Div(l, r)
//         case (l, "%",  r) => Mod(l, r)
//         case (l, "*",  r) => Times(l, r)
//         case (l, "<",  r) => LessThan(l, r)
//         case (l, "<=", r) => LessEquals(l, r)
//         case (l, "&&", r) => And(l, r)
//         case (l, "||", r) => Or(l, r)
//         case (l, "++", r) => Concat(l, r)
//         case (l, _, r)    => Equals(l, r) // Wildcard to avoid warning
//       } 
//     }
//   }

//   // match { cases+ } [match { cases+ } .... | ? ]
//   lazy val manyMatches : Syntax[List[List[MatchCase]]]  = recursive { 
//     (matchExpression ~ manyMatches.opt).map {
//       case first ~ Some(following) => first :: following
//       case first ~ None => List(first)
//     }
//   }
//   lazy val matchExpression : Syntax[List[MatchCase]] =
//     (kw("match").skip ~ "{".skip ~ oneOrMoreCases ~ "}".skip).map {
//       case cases => cases
//     }
  
//   lazy val oneOrMoreCases :  Syntax[List[MatchCase]] = recursive {
//     (kw("case").skip ~ pattern ~ "=>".skip ~ expr  ~ (oneOrMoreCases).opt).map {
//       case p ~ expr ~ Some(list) => MatchCase(p, expr) :: list
//       case p ~ expr ~ None => List(MatchCase(p, expr))
//     }
//   }
  
//   // -----------------------------------BINARY-OPERATIONS-----------------------------------------
//   lazy val binOp : Syntax[Expr] = recursive {
//     operators(factor)(
//       op("*") | op("/") | op("%") is LeftAssociative,
//       op("+") |  op("-") |  op("++") is LeftAssociative,
//       op("<") |  op("<=") is LeftAssociative,
//       op("==") is LeftAssociative,
//       op("&&") is LeftAssociative,
//       op("||") is LeftAssociative
//     ) {
//       case (l, "+",  r) => Plus(l, r)
//       case (l, "-",  r) => Minus(l, r)
//       case (l, "/",  r) => Div(l, r)
//       case (l, "%",  r) => Mod(l, r)
//       case (l, "*",  r) => Times(l, r)
//       case (l, "<",  r) => LessThan(l, r)
//       case (l, "<=", r) => LessEquals(l, r)
//       case (l, "&&", r) => And(l, r)
//       case (l, "||", r) => Or(l, r)
//       case (l, "++", r) => Concat(l, r)
//       case (l, _, r) => Equals(l, r) // Wildcard to avoid warning
//     }
//   }

//   lazy val factor: Syntax[Expr] = 
//     ((op("-") | op("!")).opt ~ simpleExpr).map {
//       case None ~ e => e
//       case Some("-") ~ e => Neg(e)
//       case Some(_) ~ e => Not(e) // Wildcard to avoid warning
//     }

//   // A literal expression.
//   lazy val literal: Syntax[Literal[_]] = accept(LiteralKind) {
//     case tk@IntLitToken(value) => IntLiteral(value) 
//     case tk@StringLitToken(value) => StringLiteral(value)   
//     case tk@BoolLitToken(value) => BooleanLiteral(value)   
//   }

//   lazy val unitLit : Syntax[Literal[_]] = 
//     ("(".skip ~ ")").map {
//       case _ => UnitLiteral() 
//     }

//   lazy val endLit : Syntax[Literal[_]] = accept(LiteralKind) {
//     case tk@IntLitToken(value) => IntLiteral(value) 
//     case tk@StringLitToken(value) => StringLiteral(value)   
//     case tk@BoolLitToken(value) => BooleanLiteral(value)   
//   }

//   // Patterns
//   lazy val pattern: Syntax[Pattern] = recursive {
//     literalPattern | wildPattern | classOrIdPattern
//   }

//   lazy val literalPattern: Syntax[Pattern] = (literal | unitLit).map { case lit => LiteralPattern(lit)}

//   lazy val wildPattern: Syntax[Pattern] = kw("_").map {case kw => WildcardPattern()}
  
//   lazy val classOrIdPattern: Syntax[Pattern] = (identifier ~ ((".".skip ~ identifier).opt ~ "(".skip ~ repsep(pattern, ",").map(_.toList) ~ ")".skip).opt).map{
//     case mod ~ Some(Some(id) ~ params) => CaseClassPattern(QualifiedName(Some(mod), id), params)
//     case id ~ Some(None ~ params) => CaseClassPattern(QualifiedName(None, id), params)
//     case id ~ None => IdPattern(id)
//   }
  

//   // Basic expressions
//   lazy val simpleExpr: Syntax[Expr] = literal.up[Expr] | variableOrCall | nestedOrUnit | error
  
//   lazy val variableOrCall: Syntax[Expr] = (identifier ~ ((".".skip ~ identifier).opt ~ "(".skip ~ repsep(expr, ",").map(_.toList) ~ ")".skip).opt).map {
//     case id ~ None => Variable(id)
//     case module ~ Some(Some(id) ~ parameters) => Call(QualifiedName(Some(module), id), parameters)
//     case id ~ Some(None ~ parameters) => Call(QualifiedName(None, id), parameters)
//   }
  
//   lazy val nestedOrUnit: Syntax[Expr] = ("(".skip ~ expr.opt ~ ")".skip).map {
//     case None => UnitLiteral()
//     case Some(expr) => expr
//   }
  
//   lazy val error: Syntax[Expr] = (kw("error") ~ "(".skip ~ value ~ ")".skip).map{
//      case kw ~ message => Error(message).setPos(kw.position)
//   }

//   // Ensures the grammar is in LL(1), otherwise prints some counterexamples
//   def checkLL1(grm : Syntax[_]): Boolean = {
    
//     if (grm.isLL1) {
//       true
//     } else {
//       debug(grm)
//       false
//     }
//   }
//   lazy val testing: Syntax[_] = expr ~<~ eof
//   override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
//     import ctx.reporter._
//     //val grm :  Syntax[_] = testing;

//     if (!checkLL1(program)) {
//       ctx.reporter.fatal("Program grammar is not LL1!")
//     }

//     // println(grm(tokens) match {
//     //   case Parsed(result, rest) => result
//     //   case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
//     //   case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
//     // });

//     program(tokens) match {
//       case Parsed(result, rest) => result
//       case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
//       case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
//     }
//   }
// }