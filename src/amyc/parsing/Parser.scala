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
    lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr) ~ "}").map {
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
  
    // A definition within a module.
    lazy val definition: Syntax[ClassOrFunDef] = functionDefinition | abstractClassDefinition | caseClassDefinition
     
    lazy val functionDefinition: Syntax[ClassOrFunDef] = 
      (kw("def") ~ identifierPos ~ "(".skip ~ parameters ~ ")".skip ~ ":".skip ~  typeTree ~ "=".skip ~ "{".skip ~ expr ~ "}".skip).map {
        case kw ~ id ~ params ~ rtype ~ body => FunDef(id._1, params, rtype, body).setPos(kw)
      }
    
    lazy val abstractClassDefinition: Syntax[ClassOrFunDef]  =
      (kw("abstract") ~ kw("class") ~ identifierPos).map {
        case kw ~ _ ~ id => AbstractClassDef(id._1).setPos(kw)
      } 
    
      def getTypes(params : List[ParamDef]) : List[TypeTree] = {
      for (p <- params) yield { 
        p match {
          case ParamDef(name, t) => t
        }
      }
    } 
    lazy val caseClassDefinition: Syntax[ClassOrFunDef] = 
      (kw("case") ~ kw("class") ~ identifierPos ~ "(".skip ~ parameters ~ ")".skip ~ kw("extends") ~ identifier).map {
        case kw ~ _ ~ id ~ params ~ _ ~ parent => CaseClassDef(id._1, getTypes(params), parent).setPos(kw)
      } 
  
    // A list of parameter definitions.
    lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)
  
    // A parameter definition, i.e., an identifier along with the expected type.
    lazy val parameter: Syntax[ParamDef] =  (identifier ~ ":".skip ~ typeTree).map {
      case id~typ => ParamDef(id,typ)
    }
  
    // A type expression.
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
    
    lazy val qualifiedIdentifier: Syntax[QualifiedName] = (identifier ~ (".".skip ~ identifier).opt).map{
      case mod ~ Some(id : String) => QualifiedName(Some(mod), id)
      case id ~ None => QualifiedName(None, id)
    }
    // A user-defined type (such as `List`).
    lazy val identifierType: Syntax[TypeTree] = qualifiedIdentifier.map {
      case qn => TypeTree(ClassType(qn))
    }
  
    // An expression.
    lazy val expr: Syntax[Expr] = recursive {
      defExpr | seqExpr
    }
    
    lazy val defExpr: Syntax[Expr] = ((kw("val") | kw("var")) ~ identifier ~ ":".skip ~ typeTree ~ "=".skip ~ value ~ ";".skip ~ expr).map {
      case KeywordToken("val") ~ id ~ tt ~ value ~ body => Let(ParamDef(id, tt), value, body, false)//.setPos(pos) 
      case KeywordToken("var") ~ id ~ tt ~ value ~ body => Let(ParamDef(id, tt), value, body, true)//.setPos(pos) 
      case default => println(default); sys.error("what")
    } 
    lazy val seqExpr: Syntax[Expr] = (value ~ (";".skip ~ expr).opt).map {
      case value ~ None => value 
      case value ~ Some(expr) => Sequence(value, expr) .setPos(value.position)
    } 
    lazy val ifExpr : Syntax[Expr] = 
      (kw("if") ~ "(".skip ~ expr ~")".skip ~ "{".skip ~ expr ~ "}".skip ~ kw("else").skip ~ "{".skip~ expr ~ "}".skip).map {
        case kw ~ cond ~ i ~ e => Ite(cond,i,e).setPos(kw.position)
      }
    
    lazy val value: Syntax[Expr] = 
      (binOp ~ manyMatches.opt).map{
        case value ~ Some(matches) => getMatchExpr(value, matches).setPos(value.position)
        case value ~ None => value
      }
  
    // Turn many consecutive matches into a match expression
    def getMatchExpr(value : Expr, matches : List[List[MatchCase]]) : Expr = {
      matches.foldLeft(value)((expr, list) => Match(expr, list).setPos(expr.position))
    }
    // match { cases+ } [match { cases+ } .... | ? ]
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
    
    lazy val oneOrMoreCases :  Syntax[List[MatchCase]] = recursive {
      (kw("case").skip ~ pattern ~ "=>".skip ~ value  ~ (oneOrMoreCases).opt).map {
        case p ~ expr ~ Some(list) => MatchCase(p, expr).setPos(p.position) :: list
        case p ~ expr ~ None => List(MatchCase(p, expr).setPos(p.position))
      }
    }
    
    // -----------------------------------BINARY-OPERATIONS-----------------------------------------
    lazy val binOp : Syntax[Expr] = recursive {
      operators(factor)(
        op("*") | op("/") | op("%") is LeftAssociative,
        op("+") |  op("-") |  op("++") is LeftAssociative,
        op("<") |  op("<=") is LeftAssociative,
        op("==") is LeftAssociative,
        op("&&") is LeftAssociative,
        op("||") is LeftAssociative
      ) {
        case (l, "+",  r) => Plus(l, r).setPos(l.position) 
        case (l, "-",  r) => Minus(l, r).setPos(l.position) 
        case (l, "/",  r) => Div(l, r).setPos(l.position) 
        case (l, "%",  r) => Mod(l, r).setPos(l.position) 
        case (l, "*",  r) => Times(l, r).setPos(l.position) 
        case (l, "<",  r) => LessThan(l, r).setPos(l.position) 
        case (l, "<=", r) => LessEquals(l, r).setPos(l.position) 
        case (l, "&&", r) => And(l, r).setPos(l.position) 
        case (l, "||", r) => Or(l, r).setPos(l.position) 
        case (l, "++", r) => Concat(l, r).setPos(l.position) 
        case (l, _, r) => Equals(l, r).setPos(l.position) // Wildcard to avoid warning
      }
    }
  
    lazy val factor: Syntax[Expr] = 
      ((op("-") | op("!")).opt ~ simpleExpr).map {
        case None ~ e => e
        case Some("-") ~ e => Neg(e).setPos(e.position)
        case Some(_) ~ e => Not(e).setPos(e.position) // Wildcard to avoid warning
      }
  
    // A literal expression.
    lazy val literal: Syntax[Literal[_]] = accept(LiteralKind) {
        case a => a match {
          case tk@IntLitToken(value) => IntLiteral(value).setPos(a.position)
          case tk@StringLitToken(value) => StringLiteral(value).setPos(a.position)
          case tk@BoolLitToken(value) => BooleanLiteral(value).setPos(a.position)   
        }
    }
  
    lazy val unitLit : Syntax[Literal[_]] = 
      ("(".skip ~ ")").map {
        case del => UnitLiteral().setPos(del.position) 
      }
  
    // Patterns
    lazy val pattern: Syntax[Pattern] = recursive {
      literalPattern | wildPattern | classOrIdPattern
    }
  
    lazy val literalPattern: Syntax[Pattern] = (literal | unitLit).map { case lit => LiteralPattern(lit).setPos(lit.position)}
  
    lazy val wildPattern: Syntax[Pattern] = kw("_").map {case kw => WildcardPattern().setPos(kw.position)}
    
    lazy val classOrIdPattern: Syntax[Pattern] = (identifierPos ~ ((".".skip ~ identifier).opt ~ "(".skip ~ repsep(pattern, ",").map(_.toList) ~ ")".skip).opt).map{
      case (mod, pos) ~ Some(Some(id) ~ params) => CaseClassPattern(QualifiedName(Some(mod), id), params).setPos(pos)
      case (id, pos) ~ Some(None ~ params) => CaseClassPattern(QualifiedName(None, id), params).setPos(pos)
      case (id, pos) ~ None => IdPattern(id).setPos(pos)
    }

    // Basic expressions
    lazy val simpleExpr: Syntax[Expr] = 
      literal.up[Expr] | variableOrCall | 
      nestedOrUnit | error | ifExpr
    
    case class VarExpr(id : Option[Name], asig : Option[Expr], params : Option[List[Expr]])

    lazy val restCall : Syntax[VarExpr] = 
      ((".".skip ~ identifier).opt ~ "(".skip ~ repsep(value, ",").map(_.toList) ~ ")".skip).map{
        case Some(id) ~ params => VarExpr(Some(id), None, Some(params))
        case None ~ params => VarExpr(None, None, Some(params))
      }

    // We would want the assigned value to be a grammar 'value', but it causes java's garbage collector to go crazy
    // For that reaason I have decided to put simpleExpr instead, as it has the same functionality using the nested 
    // expresion
    // 
    // Posible options for the expression of the assigned value:
    // value (GC Error) -> binOp (GC Error) -> factor (Correct) -> simpleExpr (Correct) 
    //
    // I believe it creates too many temporal objects while creating the grammar. Using the option Recursive doesn't 
    // solve it. Adding a terminal to the expression at the end solves it, but having double semi-colons is undesirable.
    lazy val restAsignation : Syntax[VarExpr] =
      ("=".skip ~ simpleExpr).map {
        case e => VarExpr(None, Some(e), None)
      }
      
    lazy val variableOrCall: Syntax[Expr] = recursive {
      (identifierPos ~ (restCall | restAsignation).opt).map {
        case (id, pos)    ~ None => Variable(id).setPos(pos)
        case (module,pos) ~ Some(VarExpr(Some(id), None, Some(parameters))) => Call(QualifiedName(Some(module), id), parameters).setPos(pos)
        case (id,pos)     ~ Some(VarExpr(None, None, Some(parameters))) => Call(QualifiedName(None, id), parameters).setPos(pos)
        case (id,pos)     ~ Some(VarExpr(None, Some(v), None)) => Asignation(id, v).setPos(pos)
        case (is, pos) ~ _ => sys.error("Unexpected error found during parsing.")
      }
    }
    
    lazy val nestedOrUnit: Syntax[Expr] = ("(" ~ expr.opt ~ ")".skip).map {
      case par ~ None => UnitLiteral().setPos(par.position)
      case par ~ Some(expr) => expr
    }
    
    lazy val error: Syntax[Expr] = (kw("error") ~ "(".skip ~ expr ~ ")".skip).map{
       case kw ~ message => Error(message).setPos(kw.position)
    }
  
    // Ensures the grammar is in LL(1), otherwise prints some counterexamples
    def checkLL1(grm : Syntax[_]): Boolean = {
      
      if (grm.isLL1) {
        true
      } else {
        debug(grm)
        false
      }
    }
    lazy val testing: Syntax[_] = expr ~<~ eof
    override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
      import ctx.reporter._
      
      if (!checkLL1(program))
        ctx.reporter.fatal("Program grammar is not LL1!")

      program(tokens) match {
        case Parsed(result, rest) => result
        case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
        case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
      }
    }
  }