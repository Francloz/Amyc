package amyc
package parsing

import utils._
import java.io.File

import scallion.lexical._
import scallion.input._

import amyc.utils.Position

import scala.math.BigInt

// The lexer for Amy.
object Lexer extends Pipeline[List[File], Iterator[Token]]
                with Lexers[Token, Char, SourcePosition] {


  import Tokens._

  val lexer = Lexer(
    // Keywords
    word("abstract") | word("case") | word("class") |
    word("def") | word("else") | word("extends") |
    word("if") | word("match") | word("object") |
    word("val") | word("var") | word("error") | word("_")
      |> { (cs, range) => KeywordToken(cs.mkString).setPos(range._1) },

    // Primitive type names
    word("String") | word("Int") | word("Boolean") |
    word("Unit") |> { (cs, range) => PrimTypeToken(cs.mkString).setPos(range._1) },

    // Boolean literals
    word("true") | word("false") |> { (cs, range) => BoolLitToken(cs.mkString.toBoolean).setPos(range._1) },

    // Operators
    // +  -  *  / %  <  <= && ||  ==  ++ -  !
    word(">=") | word("<=") | word("==") |
    word("++") | word("&&") | word("||") 
    | oneOf("*+-/%<>!") |> { (cs, range) => OperatorToken(cs.mkString).setPos(range._1) },
    
    // Identifiers
    // alpha alphanum*
    elem(c => ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')) ~
    many(elem(c => ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_' || ('0' <= c && c <= '9')))
    |> { (cs, range) => IdentifierToken(cs.mkString).setPos(range._1) },

    // Integer literals
    many1(elem(c => ('0' <= c && c <= '9'))) 
    |> { (cs, range) => if (cs.mkString.length < 10 && BigInt(cs.mkString) <= 2147483647) IntLitToken(cs.mkString.toInt).setPos(range._1) else ErrorToken("Int overflow (the maximum integer is 2147483647): " ++ cs.mkString).setPos(range._1) },

    // String literals
    // All characters except " in one line
    elem('\"') ~ many(elem(x => x != '\"' && x != '\n' && x != '\r')) ~ elem('\"') |> { (cs, range) => StringLitToken(cs.mkString.slice(1, cs.mkString.length - 1)).setPos(range._1) },
    // Unclosed string
    elem('\"') ~ many(elem(_ != '\"')) |> { (cs, range) => ErrorToken("Unclosed string: " ++ cs.mkString.slice(0, math.min(cs.mkString.length, 10)) ++ "...").setPos(range._1) },
    

    // Delimiters and whitespace
    // White spaces
    many1(oneOf(" \n\r\t")) |>  { (cs, range) => SpaceToken().setPos(range._1) },
    // Delimiters .,:;(){}[]= and =>
    word("=>") | oneOf(".,:;(){}[]=") |>  { (cs, range) => DelimiterToken(cs.mkString).setPos(range._1) },

    // Single line comments
    word("//") ~ many(elem(_ != '\n'))
      |> { cs => CommentToken(cs.mkString("")) },

    // Multiline comments

    // Find proper comments
    word("/*") ~ many(elem(_ != '*') | (elem('*') ~ (elem(_ != '/')))) ~ word("*/") |>  { (cs, range)  => CommentToken(cs.mkString("")) },
    
    // Find unclosed comments
    word("/*") |>  { (cs, range) => ErrorToken("Unclosed comment: " ++ cs.mkString.slice(0,  math.min(cs.mkString.length, 10)) ++ "...").setPos(range._1)},
    // Make sure that unclosed multi-line comments result in an ErrorToken.
  ) onError {
    // We also emit ErrorTokens for Scallion-handled errors.
    (cs, range) => ErrorToken(cs.mkString).setPos(range._1)
  } onEnd {
    // Once all the input has been consumed, we emit one EOFToken.
    pos => EOFToken().setPos(pos)
  }

  override def run(ctx: Context)(files: List[File]): Iterator[Token] = {
    var it = Seq[Token]().toIterator

    for (file <- files) {
      val source = Source.fromFile(file, SourcePositioner(file))
      it ++= lexer.spawn(source).filter {
        token => 
          // Remove all whitespace and comment tokens
          token match {
            case CommentToken(c) => false
            case SpaceToken() => false
            case _ => true
          }
      }.map {
        case token@ErrorToken(error) => ctx.reporter.fatal("Unknown token at " + token.position + ": " + error)
        case token => token
      }
    }
    it
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Iterator[Token], Unit] {
  override def run(ctx: Context)(tokens: Iterator[Token]): Unit = {
    tokens.foreach(println(_))
  }
}
