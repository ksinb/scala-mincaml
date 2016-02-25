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

  /*
  def r1chain[T, U](p: Parser[T], q: Parser[U])(combiner: (T, U) => T): Parser[T] =  p ~ rep(q) ^^ {
    case x ~ xs => xs.foldLeft(x){ case (a, b) => combiner(a, b) }
  }
  def right_chain[T, U](p: => Parser[T], q: => Parser[(T, U) => U], combine: (T, U) => U, first: U): Parser[U] =
    p ~ rep(q ~ p) ^^ {
    case x ~ xs => (new ~(combine, x) :: xs).foldRight(first){(_, _) match {case (f ~ a, b) => f(a, b)}}
  }
  //def cons = (x: String, y: List[Any]) => x :: y
  def hoge: Parser[Any] = chainr1("1", "|"^^{_ => (l:String, r:Any) => (l,r, "hoo")}, (l:String, r:Any) => List(l,r), "first")
*/

  lazy val File : PackratParser[Any] =
    Expression ~ chainl1(Expression, WS ^^ { _  => (l:Any, r:Any) => (l, r)})

  lazy val WS : PackratParser[String] = rep(Blank) ^^ {_=>":"}
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

  lazy val Less: PackratParser[String] = anySpace ~> "<"
  lazy val Greater :PackratParser[String] = anySpace ~> ">"
  lazy val LessEqual :PackratParser[String] = anySpace ~> "<="
  lazy val GreaterEqual :PackratParser[String] = anySpace ~> ">="
  lazy val LessGreater: PackratParser[String] = anySpace ~> "<>"
  lazy val LessMinus: PackratParser[String] = anySpace ~> "<-"

  lazy val True :PackratParser[String] = anySpace ~> "true"
  lazy val False:PackratParser[String] = anySpace ~> "false"

  lazy val If :PackratParser[String] = anySpace ~> "if"
  lazy val Then :PackratParser[String] = anySpace ~> "then"
  lazy val Else :PackratParser[String] = anySpace ~> "else"

  lazy val Let :PackratParser[String] = anySpace ~> "let"
  lazy val In :PackratParser[String] = anySpace ~> "in"
  lazy val Rec :PackratParser[String] = anySpace ~> "rec"
  lazy val ArrayCreate :PackratParser[String] = anySpace ~> "Array.create"

  lazy val Expression : PackratParser[Any] =
    CompoundExpression | EqualityExpression

  lazy val CompoundExpression : PackratParser[Any] =
    IfExpression |
    RecursiveFunctionDeclaration |
    VariableDeclaration |
    LetTupleExpression |
    ArrayCreateExpression

  lazy val IfExpression : PackratParser[Any] =
    If ~ Expression ~ Then ~ Expression ~ Else ~ Expression ^^ {
      case i~ex1~t~ex2~e~ex3 => List(ex1, ex2, ex3)
    }

  lazy val RecursiveFunctionDeclaration : PackratParser[Any] =
    Let ~ Rec ~ Ident ~ FormalArgList ~ Equal ~ Ident ~ In ~ Expression ^^ {
      case l~r~id1~f~eq~id2~i~exp => List(id1, f, id2, exp)
    }

  lazy val FormalArgList : PackratParser[Any] =
    chainl1(Ident, WS ^^ { _  => (l:Any, r:Any) => (l, r)})

  lazy val VariableDeclaration : PackratParser[Any] =
    Let ~ Ident ~ Equal ~ Expression ~ In ~ Expression ^^ {
      case l~id~e~exp1~i~exp2 => List(id, exp1, exp2)
    }

  lazy val LetTupleExpression : PackratParser[Any] =
    Let ~ LParen ~ Pat ~ RParen ~ Equal ~ Expression ~ In ~ Expression  ^^ {
      case l~lp~pat~rp~eq~exp1~i~exp2 => List(pat, exp1, exp2)
    }

  lazy val Pat : PackratParser[Any] =
    Ident ~ rep1(Comma ~> Ident) ^^ {case i~rep => i::rep}

  //done
  lazy val ArrayCreateExpression : PackratParser[Any] =
    ArrayCreate ~ ReadArrayElementExpression ~ ReadArrayElementExpression ^^ {
      case ac ~ re1 ~ re2 => List(re1, re2)
    }

  lazy val EqualityExpression: PackratParser[Any] =
    chainl1( RelationalExpression, Equal ^^ {
      case op => (l: Any, r: Any) => List(l, r) }
    )

  lazy val RelationalExpression: PackratParser[Any] =
    chainl1( AdditiveExpression, addRelationalOperator ^^ {
      case op => (l: Any, r: Any) => List(l, r) }
    )

  lazy val AdditiveExpression: PackratParser[Any] =
    chainl1( MultiplicativeExpression, (FPlus | Plus | FMinus | Minus) ^^ {
      case op => (l: Any, r: Any) => List(l, r)}
    )

  lazy val MultiplicativeExpression: PackratParser[Any] =
    chainl1( UnaryExpression, (FMul | Mul | FDiv | Div) ^^ {
      op => (l: Any, r: Any) => List(l, r)}
    )

  lazy val UnaryExpression: PackratParser[Any] =
    FunctionCall | (addUnaryOperator ~ UnaryExpression) ^^ { case op~exp => List(op, exp)}

  lazy val addUnaryOperator: PackratParser[Any] =
    FMinus| Minus //^^ {_ => "auo"}

  lazy val addRelationalOperator: PackratParser[Any] =
    (LessGreater | LessEqual | Less | GreaterEqual | Greater )

  lazy val FunctionCall: PackratParser[Any] =
    WriteArrayElementExpression ~ opt(ActualArgList) ^^ {
      case w~opt => opt match {
        case Some(o) => List(w, o)
        case None => w
      }
    }

  lazy val ActualArgList : PackratParser[Any] =
    chainl1(WriteArrayElementExpression, whiteSpace ^^ {
        op => (l:Any, r:Any) => List(l, r)}
    )

  lazy val WriteArrayElementExpression: PackratParser[Any] =
    //ReadArrayElementExpression ~ opt(Dot ~ LParen ~ Expression ~ RParen ~ LessMinus ~ Expression) ^^ {
    ReadArrayElementExpression ~ opt(WriteArrayAccess) ^^ {
      case r~opt => opt match {
        case Some(o) => List(r, o)
        case None => r
      }
    }

  lazy val WriteArrayAccess: PackratParser[Any] =
    Dot ~ LParen ~ Expression ~ RParen ~ LessMinus ~ Expression ^^ {
      case d~lp~e1~rp~lm~e2 => List(e1, e2)
    }

  lazy val ReadArrayElementExpression: PackratParser[Any] =
    SimpleExpression ~ opt(rep1(ReadArrayAccess)) ^^ {
      case s ~ rep => rep match {
        case Some(o) => s::o
        case None => s
      }
    }

  lazy val ReadArrayAccess: PackratParser[Any] =
    Dot ~ LParen ~ Expression ~ RParen ~ not(LessMinus) ^^ {
      case d~lp~e~rp~n => e
    }

  lazy val SimpleExpression: PackratParser[Any] =
     Bool |
     Number |
//   FLOAT ^^ {s => new MFloat(s.asInstanceOf[Float])} |
//   INT ^^ {s => new MInt(s.asInstanceOf[Int])} |
     Ident | //^^ {s => new MVar(s)} |
     LParen ~> Expression <~ RParen | //^^ {case l~e~r_ => e}
     LParen ~> anySpace <~ RParen ^^ {_ => new MUnit()}

  lazy val Bool: PackratParser[Any] = (True | False) ^^ {s => new MBool(s.toBoolean)}
  lazy val Number: PackratParser[String] = anySpace ~> wholeNumber
  lazy val INT: PackratParser[String] = anySpace ~> wholeNumber
  lazy val FLOAT: PackratParser[String] = anySpace ~> wholeNumber

  lazy val Ident: PackratParser[Any] =
    not(Keyword) ~> LocalID ^^ {s => new MVar(s)}

  lazy val Keyword: PackratParser[Any] =
    KeywordString ~ not(IdentifierCharacter)

  lazy val KeywordString : PackratParser[Any] =
    True | False | If | Then | Else | Let | In | Rec | ArrayCreate

  lazy val IdentifierCharacter: PackratParser[String] = regex("""[a-zA-Z0-9_]""".r)

  lazy val LocalID: PackratParser[String] =
    anySpace ~> regex("""[a-z_][a-zA-Z0-9_]*""".r)

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
   val res = parseAll(Expression, "let hoge = true in false" )

  //val res = parseAll(Expression, "false.(true).(false)" )
  // val res = parseAll(Expression, "if false then true else false" )
    println(res)
  }

}

