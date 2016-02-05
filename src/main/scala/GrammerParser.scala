/**
  * Created by Help Desk on 2016/02/04.
  */
package scalamincaml
import scala.util.parsing.combinator._
/*
object Grammar extends GrammarParser {
  //def main(args:Array[String]) {
//    println("hoge")
//    println(parseAll(SIMPLE_EXP, "()"))
    //println(parseAll(EXP, "true"))
  //  println(parseAll(EXP, "0123"))
  //  println(parseAll(EXP, "_"))
  //  println(parseAll(IDENT, "pR04"))

  }
}
*/
class GrammarParser extends RegexParsers {
  def BOOL : Parser[Any] = "true" | "false"
  def INT : Parser[Any] = """[0-9]+""".r
  //def FLOAT : Parser[Any] = INT ~  """(.[0-9]*)? ([e E][+ -]?[0-9]+)?""".r
  def NOT : Parser[Any] = "not"
  def MINUS : Parser[Any] = "-"
  def PLUS : Parser[Any] = "+"
  def MINUS_DOT : Parser[Any] = "-."
  def PLUS_DOT : Parser[Any] = "+."
  def AST_DOT : Parser[Any] = "*."
  def SLASH_DOT : Parser[Any] = "/."
  def EQUAL : Parser[Any] = "="
  def LESS_GREATER : Parser[Any] = "<>"
  def LESS_EQUAL : Parser[Any] = "<="
  def GREATER_EQUAL : Parser[Any] = ">="
  def LESS : Parser[Any] = "<"
  def GREATER : Parser[Any] = ">"
  def IF : Parser[Any] = "if"
  def THEN : Parser[Any] = "then"
  def ELSE : Parser[Any] = "else"
  def IDENT : Parser[Any] = "_" | """[a-z]([0-9]|[a-z]|[A-Z]|_)*""".r
  def LET : Parser[Any] = "let"
  def IN : Parser[Any] = "in"
  def REC : Parser[Any] = "rec"
  def COMMA : Parser[Any] = ","
  def ARRAY_CREATE : Parser[Any] = "Array.create"
  def DOT : Parser[Any] = "."
  def LESS_MINUS : Parser[Any] = "<-"
  def SEMICOLON : Parser[Any] = ";"
  def LPAREN : Parser[Any] = "("
  def RPAREN : Parser[Any] = ")"
  //def EOF

  def SIMPLE_EXP : Parser[Any] = (
    LPAREN ~ RPAREN ^^ {_ => None}
      | LPAREN ~ EXP ~ RPAREN ^^ {case lp~exp~rp => exp}

      | BOOL //^^ {_ => Bool(_)}
      | INT
      //    | FLOAT
      | IDENT
      | SIMPLE_EXP ~ DOT ~ LPAREN ~ EXP ~ RPAREN
    )

  def EXP : Parser[Any] = (
    SIMPLE_EXP
      | NOT ~ EXP
      | MINUS ~ EXP
      | PLUS ~ EXP
      | EXP ~ MINUS ~ EXP
      | EXP ~ EQUAL ~ EXP
      | EXP ~ LESS_GREATER ~ EXP
      | EXP ~ LESS  ~ EXP
      | EXP ~ GREATER  ~ EXP
      | EXP ~ LESS_EQUAL ~ EXP
      | EXP ~ GREATER_EQUAL ~ EXP
      | IF ~ EXP ~ THEN ~ EXP ~  ELSE ~ EXP
      | EXP ~ MINUS_DOT ~ EXP
      | EXP ~ PLUS_DOT ~ EXP
      | EXP ~ MINUS_DOT ~ EXP
      | EXP ~ AST_DOT ~ EXP
      | EXP ~ SLASH_DOT ~ EXP
      //    | LET ~ IDENT ~ EQUAL ~ EXP ~ IN ~ EXP
      //    | LET ~ REC ~ FUNDEF ~ IN ~ EXP
      //    | EXP ~ ACTUAL_ARGS
      //    | ELEMS
      //    | LET ~ LPAREN ~ PAT ~ RPAREN ~ EQUAL ~ EXP ~ IN ~ EXP
      | SIMPLE_EXP ~ DOT ~ LPAREN ~ EXP ~ RPAREN ~ LESS_MINUS ~ EXP
      | EXP ~ SEMICOLON ~ EXP
      | ARRAY_CREATE ~ SIMPLE_EXP ~ SIMPLE_EXP
    //    | ERROR
    )

  //def FUNDEF = IDENT ~ FORMAL_ARGS ~ EQUAL ~ EXP

  // def FORMAL_ARGS : Parser[Any] = IDENT ~ FORMAL_ARGS | IDENT

  //def ACTUAL_ARGS : Parser[Any] = ACTUAL_ARGS ~ SIMPLE_EXP | SIMPLE_EXP

  //def ELEMS : Parser[Any] = ELEMS ~ COMMA ~ EXP | EXP ~ COMMA ~ EXP

  //def PAT : Parser[Any] = PAT ~ COMMA ~ IDENT | IDENT ~ COMMA ~ IDENT

  /*
  def apply(s : String) : Unit = {
    parseAll(EXP, s) match {
      case Success(_, _) => println("Success")
      case x => println("Failure: " + x)
    }
  }
  */
}
