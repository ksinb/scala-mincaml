/**
  * Created by Help Desk on 2016/02/04.
  */

import scala.util.parsing.combinator._

object Grammar {
  def main(args: Array[String]) = {
    new Grammar().parse
  }
}

class Grammar extends JavaTokenParsers with PackratParsers {
  override def skipWhitespace = false

//  def parse = parseAll(hoge, input)

  def r1chain[T, U](p: Parser[T], q: Parser[U])(combiner: (T, U) => T): Parser[T] =  p ~ rep(q) ^^ {
    case x ~ xs => xs.foldLeft(x){ case (a, b) => combiner(a, b) }
  }

/*
  def right_chain[T, U](p: => Parser[T], q: => Parser[(T, U) => U], combine: (T, U) => U, first: U): Parser[U] =
    p ~ rep(q ~ p) ^^ {
    case x ~ xs => (new ~(combine, x) :: xs).foldRight(first){(_, _) match {case (f ~ a, b) => f(a, b)}}
  }
*/

  //def cons = (x: String, y: List[Any]) => x :: y
  def hoge: Parser[Any] = chainr1("1", "|"^^{_ => (l:String, r:Any) => (l,r, "hoo")}, (l:String, r:Any) => List(l,r), "first")


  lazy val File : PackratParser[Any] = Expression ~ chainl1(Expression, WS ^^ { _  => (l:Any, r:Any) => (l, r)})

  lazy val WS : PackratParser[String] = rep(Blank) ^^{_=>":"}
  lazy val Blank = " " | "\n" | "\t" | "\r"
  lazy val anySpace: PackratParser[String] = rep("""\s""".r)  ^^ { _.mkString }

  lazy val Plus: PackratParser[String] = anySpace ~> "+"
  lazy val Minus: PackratParser[String] = anySpace ~> "-"
  lazy val Mul: PackratParser[String] = anySpace ~> "*"
  lazy val Div: PackratParser[String] = anySpace ~> "/"

  lazy val FPlus: PackratParser[String] = anySpace ~> "+."
  lazy val FMinus: PackratParser[String] = anySpace ~> "-."
  lazy val FMul: PackratParser[String] = anySpace ~> "*."
  lazy val FDiv: PackratParser[String] = anySpace ~> "/."

  lazy val Equal: PackratParser[String] = anySpace ~> "="
  //lazy val LogicalNot: PackratParser[String] = anySpace ~> "!"

  lazy val Less: PackratParser[String] = anySpace ~>"<"
  lazy val Greater:PackratParser[String] = anySpace ~> ">"
  lazy val True:PackratParser[String] = anySpace ~> "true"
  lazy val False = "false"
  lazy val If = "if"
  lazy val Then = "then"
  lazy val Else = "else"
  lazy val Let:PackratParser[String] = anySpace ~> "let"
  lazy val In = "in"
  lazy val Rec = "rec"
  lazy val ArrayCreate = "Array.create"

  lazy val Keyword : PackratParser[String] =
    True | False | If | Then | Else | Let | In | Rec | ArrayCreate

  lazy val Expression : PackratParser[Any] =
    CompoundExpression | EqualityExpression

  lazy val CompoundExpression : PackratParser[Any] =
    IfExpression |
    RecursiveFunctionDeclaration |
    VariableDeclaration |
    LetTupleExpression |
    ArrayCreateExpression

  lazy val IfExpression : PackratParser[Any] =
    If ~ WS ~ Expression ~ WS ~ Then ~ WS ~ Expression ~ WS ~ Else ~ WS ~ Expression

  //done
  lazy val RecursiveFunctionDeclaration : PackratParser[Any] =
    Let ~ WS ~ Rec ~ WS ~ Ident ~ WS ~ FormalArgList ~ WS ~  "=" ~ WS ~ Ident ~ WS ~ "in" ~ WS ~ Expression ^^ {
      _ => "RFD"
    }

  lazy val FormalArgList : PackratParser[Any] =
    chainl1(Ident, WS ^^ {
      _  => (l:Any, r:Any) => (l, r)}
    )

  //done
  lazy val VariableDeclaration : PackratParser[Any] =
    Let ~ WS ~ Ident ~ WS ~ "=" ~ WS ~ Expression ~ WS ~ "in" ~ WS ~ Expression ^^ {
      _ => "VD"
    }

  //done
  lazy val LetTupleExpression : PackratParser[Any] =
    Let ~ WS ~ "(" ~ WS ~ Pat ~ WS ~ ")" ~ WS ~  "=" ~ WS ~ Expression ~ WS ~ "in" ~ WS ~ Expression  ^^ {
      _ => "LTE"
    }

  lazy val Pat : PackratParser[Any] =
    Ident ~ rep1(WS ~ Ident ~ WS ~ Ident)

  //done
  lazy val ArrayCreateExpression : PackratParser[Any] =
    ArrayCreate ~ WS ~ ReadArrayElementExpression ~ WS ~ ReadArrayElementExpression ^^ {
      _ => "ACE"
    }

  lazy val EqualityExpression: PackratParser[Any] =
    chainl1( RelationalExpression, Equal ^^ {
      case op => (l: String, r: String) => l+op+r }
    )

  lazy val RelationalExpression: PackratParser[String] =
    chainl1( AdditiveExpression, addRelationalOperator ^^ {
      case op => (l: String, r: String) => l+op+r }
    )

  lazy val AdditiveExpression: PackratParser[String] =
    chainl1( MultiplicativeExpression,  (FPlus | Plus | FMinus | Minus) ^^ {
      case op => (l: String, r: String) => l+op+r}
    )

  lazy val MultiplicativeExpression: PackratParser[String] =
    chainl1(UnaryExpression, (FMul | Mul | FDiv | Div) ^^ {
      op => (l: String, r: String) => l+op+r}
    )

  lazy val addUnaryOperator: PackratParser[String] =
    (FMinus| Minus) ^^ {_ => "auo"} // | LogicalNot

  lazy val UnaryExpression: PackratParser[String] =
    FunctionCall | (addUnaryOperator ~ UnaryExpression) ^^ { case a~u => a+"un"+u}

  lazy val addRelationalOperator: PackratParser[String] =
    ("<>" | "<=" | "<" | ">=" | ">" ) ^^ {_=>"aro"}

  //heeeeeeeeee
  lazy val FunctionCall: PackratParser[String] =
    WriteArrayElementExpression ~ opt(ActualArgList) ^^ { case w~opt => w+"hoge"+opt}

  //heeeeeeeee
  lazy val ActualArgList : PackratParser[String] =
    chainl1(WriteArrayElementExpression, whiteSpace ^^ {
        op => (l:String, r:String) => l+r }
    )

  lazy val WriteArrayElementExpression: PackratParser[String] =
    ReadArrayElementExpression ~ opt("." ~ "(" ~  Expression ~ ")" ~ "<-" ~ Expression) ^^ {_ => "WAEE"}

  lazy val ReadArrayElementExpression: PackratParser[Any] =
    SimpleExpression ~ rep( "." ~ "(" ~ Expression ~ ")" ~ not("<-")) ^^ {_=>"RAEE"}

  lazy val SimpleExpression: PackratParser[Any] =
    Bool | Number | Ident | ("("~Expression~")" | "("~anySpace~")") ^^{_=>"SP"}

  lazy val Bool: PackratParser[String] = True | False
  lazy val Number: PackratParser[String] = wholeNumber//rep(DIGIT)
  lazy val Ident = "hoge" //not(Keyword)


  def pr = "pr"
  def opti = rep(pr)~not("ho")

  //ArrayCreate ~ WS ~ ReadArrayElementExpression ~ WS ~ ReadArrayElementExpression ^^ {
  def parse = {
    //val res = parseAll(opti, "")
    val res = parseAll(Expression, "hoge(3)")
    println(res)
  }



/*


  def BOOL : Parser[String] = "true" | "false"
  def INT : Parser[String] = [0-9]+""".r// ^^ {_ => "int"}
  def FLOAT : Parser[String] = floatingPointNumber //INT ~  """(.[0-9]*)? ([e E][+ -]?[0-9]+)?""".r
  def NOT : Parser[String] = "not" ^^ {_ => "hee"}
  def MINUS : Parser[String] = "-"
  def PLUS : Parser[String] = "+"
  def MINUS_DOT : Parser[String] = "-."
  def PLUS_DOT : Parser[String] = "+."
  def AST_DOT : Parser[String] = "*."
  def SLASH_DOT : Parser[String] = "/."
  def EQUAL : Parser[String] = "="
  def LESS_GREATER : Parser[String] = "<>"
  def LESS_EQUAL : Parser[String] = "<="
  def GREATER_EQUAL : Parser[String] = ">="
  def LESS : Parser[String] = "<"
  def GREATER : Parser[String] = ">"
  def IF : Parser[String] = "if"
  def THEN : Parser[String] = "then"
  def ELSE : Parser[String] = "else"
  def IDENT : Parser[String] = "_" | """[a-z]([0-9]|[a-z]|[A-Z]|_)*""".r ^^ {_ => "idddddddd"}
  def LET : Parser[String] = "let"
  def IN : Parser[String] = "in"
  def REC : Parser[String] = "rec"
  def AND : Parser[String] = "and"
  def COMMA : Parser[String] = ","
  def ARRAY_CREATE : Parser[String] = "Array.create"
  def DOT : Parser[String] = "."
  def LESS_MINUS : Parser[String] = "<-"
  def SEMICOLON : Parser[String] = ";"
  def LPAREN : Parser[String] = "("
  def RPAREN : Parser[String] = ")"
  //def EOF

// "-" ^^ {op => (left:Any, right:Any) => (op, left, right)}
  def SIMPLE_EXP : Parser[String] = chainl1(
    SIMPLE_TERM,
    DOT ~ LPAREN ~ EXP ~ RPAREN ^^ {op => (l:Any, r:Any) => (l, r)}
  )

  def SIMPLE_TERM : Parser[String] = (
      LPAREN ~ EXP ~ RPAREN
      | LPAREN ~ RPAREN
      | BOOL
      | INT
      | FLOAT
      | IDENT
    )

  def EXP : Parser[String] = chainl1(
    TERM,

    (   AST_DOT | SLASH_DOT
      | PLUS | MINUS | PLUS_DOT | MINUS_DOT
      | EQUAL | LESS_GREATER | LESS | GREATER | LESS_EQUAL | GREATER_EQUAL
      | SEMICOLON) ^^ {op => (l:Any, r:Any) => (op, l, r)}
  )
      //    | LET ~ IDENT ~ EQUAL ~ EXP ~ IN ~ EXP
      //    | LET ~ REC ~ FUNDEF ~ IN ~ EXP
      //    | EXP ~ ACTUAL_ARGS
      //    | ELEMS
      //    | LET ~ LPAREN ~ PAT ~ RPAREN ~ EQUAL ~ EXP ~ IN ~ EXP


      //| SEMICOLON ~ EXP_REST
    //    | ERROR

  def TERM : Parser[String] = (
       NOT ~ EXP
      | SIMPLE_EXP
      | MINUS ~ EXP
      | IF ~ EXP ~ THEN ~ EXP ~  ELSE ~ EXP
      | MINUS_DOT ~ EXP
      | SIMPLE_EXP ~ DOT ~ LPAREN ~ EXP ~ RPAREN ~ LESS_MINUS ~ EXP
      | ARRAY_CREATE ~ SIMPLE_EXP ~ SIMPLE_EXP
   )
//assignment-expression
//single-assignment-expression
//single-variable-assignment-expression
//operator-expression
//conditional-operator-expression
//range-constructor
//operator-OR-expression
//operator-AND-expression
//equality-expression
  lazy val additiveExpression: Parser[String] =
    chainl1 ( multiplicativeExpression, (PLUS | MINUS) ^^ {
      case n~op => (l: String, r: String) => "("+l+" "+op+" "+r+")" }
    )

  lazy val multiplicativeExpression: Parser[String] =
    chainl1 ( unaryMinusExpression,  (AST_DOT | SLASH_DOT) ^^ {
      case n~op => (l: String, r:String) => "("+l+op+r+")" }
    )

  lazy val unaryMinusExpression: Parser[String] =
    unaryExpression  | MINUS ~ unaryExpression ^^ { case m~p => "-"+p }

  lazy val unaryExpression: Parser[String] =
    primaryExpression |
      PLUS ~ unaryExpression ^^ { case p~u => "+"+u } |
      NOT ~ unaryExpression ^^ { case n~u => "!"+u }

  lazy val primaryExpression: Parser[String] = memo(
    r1chain ( primaryExpression1,
      not(WS) ~ DoubleCOLON ~ ConstID ^^ { case n~d~c => "::" + c } | //scoped-constant-reference

        not(LineTerminator) ~ PERIOD ~ methodName ~ not(argumentWithoutParentheses) ~
          ??(argumentWithParentheses) ~ ??(block) ^^ {
          case n1~p~m~n2~a~b => "." + m + a + b } | //primary-method-invocation

        not(LineTerminator) ~ DoubleCOLON ~ methodName ~ argumentWithParentheses ~ ??(block) ^^ {
          case n~d~m~a~b => "::" + m + a + b } | //primary-method-invocation

        not(LineTerminator) ~ DoubleCOLON ~ methodNameExceptConstant ~ ??(block) ^^ {
          case n~d~m~b => "::" + m + b } | //primary-method-invocation

        not(WS) ~ LBRACKET ~ ??(indexingArgumentList) ~ RBRACKET ~ not(EQUAL) ^^ {
          case n1~l~i~r~n2 => "["+i+"]" } //indexing-method-invocation
    ) { case (x, y) => "(" + x + y + ")" } )

  lazy val primaryExpression1: Parser[String] =
      fundefs

      classDefinition |
      singletonClassDefinition |
      moduleDefinition |
      methodDefinition |
      singletonMethodDefinition |
      yieldWithOptionalArgument |
      unlessExpression |
      caseExpression |
      whileExpression |
      untilExpression |
      forExpression |
      returnWithoutArgument |
      breakWithoutArgument |
      nextWithoutArgument |
      redoExpression |
      retryExpression |
      beginExpression |
      arrayConstructor |
      hashConstructor |
      definedWithParentheses |
      groupingExpression |
      scopedConstantReference |
      primaryMethodInvocation ^^ { case p => "primary-method-inv("+p+")" }|
      literal |
      variableReference

  lazy val fundefs =
    fundef ~ AND ~ fundefs | fundef

  lazy val fundef =
    IDENT ~ FORMAL_ARGS ~ EQUAL ~ EXP

  lazy val FORMAL_ARGS : Parser[String] =
    IDENT ~ FORMAL_ARGS | IDENT

  lazy val ACTUAL_ARGS : Parser[String] =
    repsep(SIMPLE_EXP, "")

  lazy val ELEMS : Parser[String] =
    repsep(EXP, COMMA)

  lazy val PAT : Parser[String] =
    repsep(IDENT, COMMA)
    */
}

