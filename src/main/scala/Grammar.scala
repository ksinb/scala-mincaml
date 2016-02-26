/**
  * Created by Help Desk on 2016/02/04.
  */
package scala_mincaml

import scala.util.parsing.combinator._

object Grammar {
  def main(args: Array[String]) = {
    new Grammar().parse
  }
}

class Grammar extends JavaTokenParsers with PackratParsers {
  override def skipWhitespace = false


 def r1chain[T, U](p: Parser[T], q: Parser[U])(combiner: (T, U) => T): Parser[T] =  p ~ rep(q) ^^ {
    case x ~ xs => xs.foldLeft(x){ case (a, b) => combiner(a, b) }
  }
  def right_chain[T, U](p: => Parser[T], q: => Parser[(T, U) => U], combine: (T, U) => U, first: U): Parser[U] =
    p ~ rep(q ~ p) ^^ {
    case x ~ xs => (new ~(combine, x) :: xs).foldRight(first){(_, _) match {case (f ~ a, b) => f(a, b)}}
  }
  //def cons = (x: String, y: List[Any]) => x :: y
  def hoge: Parser[Any] = chainr1("1", "|" ^^ { _ => (l:String, r:java.io.Serializable) => (l,r)}, (l:String, r:java.io.Serializable) => (l, r), List())


  lazy val File : PackratParser[Any] =
    Expression ~ chainl1(Expression, WS ^^ { _  => (l:Any, r:Any) => (l, r)})

  lazy val WS : PackratParser[Any] = rep(Blank)
  lazy val Blank = " " | "\n" | "\t" | "\r"
  lazy val anySpace: PackratParser[String] = rep("""\s""".r) ^^ { _.mkString }

  lazy val Plus: PackratParser[String] = anySpace ~> "+"
  lazy val Minus: PackratParser[String] = anySpace ~> "-"
  lazy val Mul: PackratParser[String] = anySpace ~> "*"
  lazy val Div: PackratParser[String] = anySpace ~> "/"

  lazy val FPlus: PackratParser[String] = anySpace ~> "+."
  lazy val FMinus: PackratParser[String] = anySpace ~> "-."
  lazy val FMul: PackratParser[String] = anySpace ~> "*."
  lazy val FDiv: PackratParser[String] = anySpace ~> "/."

  lazy val Equal: PackratParser[String] = anySpace ~> "="
  lazy val LParen: PackratParser[String] = anySpace ~> "("
  lazy val RParen: PackratParser[String] = anySpace ~> ")"

  lazy val Dot: PackratParser[String] = anySpace ~> "."
  lazy val Comma: PackratParser[String] = anySpace ~> ","
  lazy val SemiColon: PackratParser[String] = anySpace ~> ";"

  lazy val Less: PackratParser[String] = anySpace ~> "<"
  lazy val Greater :PackratParser[String] = anySpace ~> ">"
  lazy val LessEqual :PackratParser[String] = anySpace ~> "<="
  lazy val GreaterEqual :PackratParser[String] = anySpace ~> ">="
  lazy val LessGreater: PackratParser[String] = anySpace ~> "<>"
  lazy val LessMinus: PackratParser[String] = anySpace ~> "<-"

  lazy val True :PackratParser[String] = anySpace ~> "true"
  lazy val False:PackratParser[String] = anySpace ~> "false"

  lazy val Not :PackratParser[String] = anySpace ~> "not"

  lazy val If :PackratParser[String] = anySpace ~> "if"
  lazy val Then :PackratParser[String] = anySpace ~> "then"
  lazy val Else :PackratParser[String] = anySpace ~> "else"

  lazy val Let :PackratParser[String] = anySpace ~> "let"
  lazy val In :PackratParser[String] = anySpace ~> "in"
  lazy val Rec :PackratParser[String] = anySpace ~> "rec"
  lazy val And :PackratParser[String] = anySpace ~> "and"
  lazy val ArrayCreate :PackratParser[String] = anySpace ~> "Array.create"

  lazy val addRelationalOperator: PackratParser[String] =
    LessGreater | LessEqual | Less | GreaterEqual | Greater

  /*
  // 10.1
  lazy val statementList: PackratParser[Any] =
    repsep(statement, SemiColon)

  //12
  lazy val statement: PackratParser[Any] =
    repsep(statement1, SemiColon)

  lazy val statement1: PackratParser[Any] =
    assignmentStatement |
    expressionStatement |
    IfExpression |
    RecursiveFunctionDeclaration |
    VariableDeclaration |
    LetTupleExpression |
    ArrayCreateExpression


  //12.2
  lazy val expressionStatement: PackratParser[Any] =
    expression

  // 11.1
  lazy val expression: PackratParser[Any] =
  chainl1(notExpression, And ^^ {_ => (l:Any, r:Any) => List(l,r) })

  lazy val notExpression: PackratParser[Any] =
    keywordNotExpression |
    methodInvocationWithoutParentheses |
    operatorExpression

  lazy val keywordNotExpression: PackratParser[Any] =
    Not ~ notExpression

  //
  lazy val methodInvocationWithoutParentheses: Parser[String] = memo(
    command

  lazy val command: Parser[String] =
      primaryExpression ~ not(LineTerminator) ~ (PERIOD | DoubleCOLON) ~ methodName ~ not(not(WS) ~ LPAREN) ~
        argumentWithoutParentheses ^^ { case p~n1~c~m~n2~a => "command("+p+c+m+" "+a+")" }

  // 11.4
  lazy val operatorExpression: PackratParser[Any] =
    assignmentExpression |


  // 11.4.2 Assignments

  lazy val assignmentExpression: Parser[String] =
    singleAssignmentExpression |

  lazy val assignmentStatement: Parser[String] =
    singleAssignmentStatement |

  // 11.4.2.2 Single assignments

  lazy val singleAssignmentExpression: Parser[String] =
    singleVariableAssignmentExpression |

  lazy val singleAssignmentStatement: Parser[String] =
    singleVariableAssignmentStatement |

  // 11.4.2.2.2 Single variable assignments

  lazy val singleVariableAssignmentStatement: Parser[String] =
    variable ~ not(LineTerminator) ~ EQUAL ~ methodInvocationWithoutParentheses ^^ {
      case v~n~e~m => "single-variable-st("+v+"="+m+")" }

  lazy val singleVariableAssignmentExpression: Parser[String] =
    variable ~ not(LineTerminator) ~ EQUAL ~ operatorExpression ^^ {
      case v~n~e~o => "single-variable-ex("+v+"="+o+")" }


  // 11.4.4 Binary operator expressions
  lazy val equalityExpression: Parser[String] = memo(
    relationalExpression ~ ??(relationalOperation)  ^^ { case e~r => e+r } )

  lazy val relationalOperation: Parser[String] =
     relationalOperator ~ relationalExpression ^^ { case n~o~e => o+e }

  lazy val relationalOperator: Parser[String] =
    LtEqGT | DoubleEQ | TripleEQ | NotEQ | EqTILDE | NotTILDE

  lazy val relationalExpression: Parser[String] =
    chainl1 ( additiveExpression, not(LineTerminator) ~ (GT | GtEQ | LT | LtEQ) ^^ {
      case n~op => (l: String, r: String) => "("+ l+" "+op+" "+r +")" } )

  lazy val additiveExpression: Parser[String] =
    chainl1 ( multiplicativeExpression, not(LineTerminator) ~ (PLUS | MINUS) ^^ {
      case n~op => (l: String, r: String) => "("+ l+" "+op+" "+r +")" } )

  lazy val multiplicativeExpression: Parser[String] =
    chainl1 ( unaryMinusExpression, not(LineTerminator) ~ (MULT | DIV | PERCENT) ^^ {
      case n~op => (l: String, r:String) => "(" + l+op+r +")" })

  // 11.4.3 Unary operator expressions
  lazy val unaryMinusExpression: Parser[String] =
    unaryExpression  |
    MINUS ~ unaryExpression ^^ { case m~p => "-"+p }

  lazy val unaryExpression: Parser[String] =
    primaryExpression |
    NotOP ~ unaryExpression ^^ { case n~u => "!"+u }

  // 11.5 Primary expressions
  lazy val primaryExpression: PackratParser[Any] =
    rep(primaryExpression1)

  lazy val primaryExpression1: PackratParser[Any] =
    IfExpression |
    RecursiveFunctionDeclaration |
    VariableDeclaration |
    LetTupleExpression |
    ArrayCreateExpression

*/
  //########################################
  lazy val Expression : PackratParser[T] =
    //repsep(Statement, SemiColon)
  Statement

  lazy val Statement : PackratParser[T] =
    IfExpression |
    //RecursiveFunctionDeclaration |
    VariableDeclaration |
    LetTupleExpression |
    ArrayCreateExpression |
    EqualityExpression |
    SimpleExpression

  lazy val SimpleExpression: PackratParser[MBool] =
    Bool //|
  //     Number |
  //   FLOAT ^^ {s => new MFloat(s.asInstanceOf[Float])} |
  //   INT ^^ {s => new MInt(s.asInstanceOf[Int])} |
  //     Ident | //^^ {s => new MVar(s)} |
  //     LParen ~> Expression <~ RParen | //^^ {case l~e~r_ => e}
  //     LParen ~> anySpace <~ RParen ^^ {_ => new MUnit()}

  lazy val Bool: PackratParser[MBool] = (True | False) ^^ {s => new MBool(s.toBoolean)}
  lazy val Number: PackratParser[String] = anySpace ~> wholeNumber
  lazy val INT: PackratParser[String] = anySpace ~> wholeNumber
  lazy val FLOAT: PackratParser[String] = anySpace ~> wholeNumber

  lazy val Ident: PackratParser[MVar] =
    not(Keyword) ~> LocalID ^^ {s => new MVar(s)}

  lazy val Keyword: PackratParser[Any] =
    KeywordString ~ not(IdentifierCharacter)

  lazy val KeywordString : PackratParser[String] =
    True | False | If | Then | Else | Let | In | Rec | ArrayCreate

  lazy val IdentifierCharacter: PackratParser[String] = regex("""[a-zA-Z0-9_]""".r)

  lazy val LocalID: PackratParser[String] =
    anySpace ~> regex("""[a-z_][a-zA-Z0-9_]*""".r)

  lazy val IfExpression : PackratParser[MIf] =
    If ~ Expression ~ Then ~ Expression ~ Else ~ Expression ^^ {
      case i ~ ex1 ~ t ~ ex2 ~ e ~ ex3 => new MIf(ex1, ex2, ex3)
    }

/*
  // LET REC fundefs IN exp
  lazy val RecursiveFunctionDeclaration : PackratParser[MLetRec] =
    Let ~ Rec ~ Fundefs ~ In ~ Expression ^^ {
      case lt ~ rc ~ fd ~ in ~ exp => new MLetRec(fd, exp)
    }
*/
  //fundefs:
  //| fundef AND fundefs
  //| fundef
  lazy val Fundefs: PackratParser[Any] =
    chainl1(Fundef, And ^^ { _  => (l:Any, r:Any) => List(l, r)})

  //fundef:
  //| IDENT formal_args EQUAL exp
  lazy val Fundef: PackratParser[Any] =
    Ident ~ FormalArgList ~ Equal ~ Ident ^^ {
      _ => (l:List[T], r:T) => List(l, r)
    }

  lazy val FormalArgList : PackratParser[List[T]] =
    repsep(Ident, WS) ^^ {List() ++ _}

  lazy val VariableDeclaration : PackratParser[MLet] =
    Let ~ Ident ~ Equal ~ Expression ~ In ~ Expression ^^ {
      case l~id~e~exp1~i~exp2 => new MLet(id, exp1, exp2)
    }

  lazy val LetTupleExpression : PackratParser[MLetTuple] =
    Let ~ LParen ~ Pat ~ RParen ~ Equal ~ Expression ~ In ~ Expression  ^^ {
      case l~lp~pat~rp~eq~exp1~i~exp2 => new MLetTuple(pat, exp1, exp2)
    }

  lazy val Pat : PackratParser[List[T]] =
    Ident ~ rep1(Comma ~> Ident) ^^ {case i~rep => i::rep}

  lazy val ArrayCreateExpression : PackratParser[MArray] =
    ArrayCreate ~ Expression ~ Expression ^^ {
      case ac ~ re1 ~ re2 => new MArray(re1, re2)
    }

  lazy val EqualityExpression: PackratParser[T] =
    chainl1( RelationalExpression, Equal ^^ {
      case op => (l: T, r: T) => new MEq(l,r)}
    )

  lazy val RelationalExpression: PackratParser[T] =
    chainl1( AdditiveExpression, addRelationalOperator ^^ {
      case op => (l: T, r: T) => new MAdd(l,r)}
    )

  lazy val AdditiveExpression: PackratParser[T] =
    chainl1( MultiplicativeExpression, (FPlus | Plus | FMinus | Minus) ^^ {
      case op => (l: T, r: T) => new MFAdd(l,r)}
    )

  lazy val MultiplicativeExpression: PackratParser[T] =
    chainl1( UnaryExpression, (FMul | Mul | FDiv | Div) ^^ {
      case op => (l: T, r: T) => new MFMul(l,r)}
    )

  lazy val UnaryExpression: PackratParser[T] =
    FMinus ~ UnaryExpression ^^ { case op~exp => MFNeg(exp)} |
    Minus ~ UnaryExpression ^^ { case op~exp => MNeg(exp)}
    //applyFunction

  /*
  lazy val applyFunction: PackratParser[T] =
    Expression ~ ActualArgList ^^ {case e~a => new MApp(e, a)}

  lazy val ActualArgList : PackratParser[List[T]] =
    chainl1(SimpleExpression, whiteSpace ^^ {
      op => (l:T, r:T) => l::List(r)}
    )
*/

  lazy val ActualArgList : PackratParser[Any] =
    chainl1("a"|"b"|"c", "." ^^ {
     case op => (l:String, r:String) => l+"o"+r}
    )
  /*
  lazy val FunctionCall: PackratParser[T] =
    WriteArrayElementExpression ~ opt(ActualArgList) ^^ {
      case w~opt => opt match {
        case Some(o) => w:::o
        case None => w
      }
    }

  lazy val ActualArgList : PackratParser[List[T]] =
    chainl1(WriteArrayElementExpression, whiteSpace ^^ {
        op => (l:List[T], r:List[T]) => l:::r}
    )

  lazy val WriteArrayElementExpression: PackratParser[List[T]] =
    //ReadArrayElementExpression ~ opt(Dot ~ LParen ~ Expression ~ RParen ~ LessMinus ~ Expression) ^^ {
    ReadArrayElementExpression ~ opt(WriteArrayAccess) ^^ {
      case r~opt => opt match {
        case Some(o) => r:::o
        case None => r
      }
    }

  lazy val WriteArrayAccess: PackratParser[List[T]] =
    Dot ~ LParen ~ Expression ~ RParen ~ LessMinus ~ Expression ^^ {
      case d~lp~e1~rp~lm~e2 => List(e1, e2)
    }

  lazy val ReadArrayElementExpression: PackratParser[T] =
    SimpleExpression ~ opt(rep1(ReadArrayAccess)) ^^ {
      case s ~ rep => rep match {
        case Some(o) => s::o
        case None => s
      }
    }

  lazy val ReadArrayAccess: PackratParser[T] =
    Dot ~ LParen ~ Expression ~ RParen ~ not(LessMinus) ^^ {
      case d~lp~e~rp~n => e
    }
*/

 /*// AST CONSTRUCTION
 lazy val TPat =
   repsep("ab", ",") ^^ { case l:List[String] => l.map({_ => MInt("123")}) }

  lazy val TList : PackratParser[Any] =
    chainl1(hoge, "+" ^^ {
      op => (l:Any, r:Any) => MAdd(l, r) }
    )
  lazy val hoge: PackratParser[String] = anySpace~>"hoge"
*/
  def parse = {
   val res = parseAll(FormalArgList, "hoge1 hage1 hoge2 hage2" )
   //val res = parseAll(Expression, "Array.create true false"  )
  //val res = parseAll(Expression, "false.(true).(false)" )
   //val res = parseAll(Expression, "if false then true else false" )
    println(res)
  }

}

