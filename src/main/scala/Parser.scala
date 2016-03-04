/**
  * Created by Help Desk on 2016/02/04.
  */
package scala_mincaml

import scala.util.parsing.combinator._

object Parser {
  def main(args: Array[String]) = {
    new Parser().parse
  }
}

class Parser extends Syntax with JavaTokenParsers with PackratParsers{
  override def skipWhitespace = false

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
  lazy val FMUL: PackratParser[String] = anySpace ~> "*."
  lazy val FDIV: PackratParser[String] = anySpace ~> "/."

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

  lazy val NOT :PackratParser[String] = anySpace ~> "not"

  lazy val IF :PackratParser[String] = anySpace ~> "if"
  lazy val Then :PackratParser[String] = anySpace ~> "then"
  lazy val Else :PackratParser[String] = anySpace ~> "else"

  lazy val LET :PackratParser[String] = anySpace ~> "let"
  lazy val In :PackratParser[String] = anySpace ~> "in"
  lazy val Rec :PackratParser[String] = anySpace ~> "rec"
  lazy val And :PackratParser[String] = anySpace ~> "and"
  lazy val ArrayCreate :PackratParser[String] = anySpace ~> "Array.create"

  lazy val addRelationalOperator: PackratParser[String] =
    LessGreater | LessEqual | Less | GreaterEqual | Greater

  lazy val Expression : PackratParser[T] =
  //repsep(Statement, SemiColon)
    CompaundExpression | EqualityExpression

  lazy val CompaundExpression : PackratParser[T] =
    IfExpression |
      RecursiveFunctionDeclaration |
      VariableDeclaration |
      LetTupleExpression |
      ArrayCreateExpression

  lazy val BOOL: PackratParser[Bool] = (True | False) ^^ {s => new Bool(s.toBoolean)}
  lazy val Number: PackratParser[String] = anySpace ~> wholeNumber
  lazy val INT: PackratParser[String] = anySpace ~> wholeNumber
  lazy val FLOAT: PackratParser[String] = anySpace ~> wholeNumber

  lazy val Ident: PackratParser[Id.T] =
    not(Keyword) ~> LocalID ^^ {new Id.T(_)}

  lazy val Keyword: PackratParser[Any] =
    KeywordString ~ not(IdentifierCharacter)

  lazy val KeywordString : PackratParser[String] =
    True | False | IF | Then | Else | LET | In | Rec | ArrayCreate

  lazy val IdentifierCharacter: PackratParser[String] = regex("""[a-zA-Z0-9_]""".r)

  lazy val LocalID: PackratParser[String] =
    anySpace ~> regex("""[a-z_][a-zA-Z0-9_]*""".r)

  lazy val SimpleExpression: PackratParser[T] =
      BOOL |
      //     Number |
      //   FLOAT ^^ {s => new MFloat(s.asInstanceOf[Float])} |
      //   INT ^^ {s => new MInt(s.asInstanceOf[Int])} |
      Ident ^^ {new Var(_)} |
      LParen ~> Expression <~ RParen //|
      //LParen ~> anySpace <~ RParen ^^ {new MUnit(_)}

  lazy val IfExpression : PackratParser[If] =
    IF ~ Expression ~ Then ~ Expression ~ Else ~ Expression ^^ {
      case i ~ ex1 ~ t ~ ex2 ~ e ~ ex3 => If(ex1, ex2, ex3)
    }

  lazy val RecursiveFunctionDeclaration : PackratParser[LetRec] =
    LET ~ Rec ~ Fundefs ~ In ~ Expression ^^ {
      case lt ~ rc ~ fd ~ in ~ exp => LetRec(fd, exp)
    }

  lazy val Fundefs: PackratParser[List[Fundef]] =
    repsep(FUNDEF, And) ^^ {List() ++ _}

  lazy val FUNDEF: PackratParser[Fundef] =
    Ident ~ FormalArgList ~ Equal ~ Expression ^^ {
      case id1 ~ arg ~ eq ~ id2 => new Fundef(addtyp(id1), arg, id2)
    }

  lazy val FormalArgList : PackratParser[List[(Id.T, Type.T)]] =
    repsep(Ident ^^ {addtyp(_)}, WS) ^^ {List() ++ _}

  lazy val VariableDeclaration : PackratParser[Let] =
    LET ~ Ident ~ Equal ~ Expression ~ In ~ Expression ^^ {
      case l~id~e~exp1~i~exp2 => new Let(addtyp(id), exp1, exp2)
    }

  lazy val LetTupleExpression : PackratParser[LetTuple] =
    LET ~ LParen ~ Pat ~ RParen ~ Equal ~ Expression ~ In ~ Expression  ^^ {
      case l~lp~pat~rp~eq~exp1~i~exp2 => new LetTuple(pat, exp1, exp2)
    }

  lazy val Pat : PackratParser[List[(Id.T, Type.T)]] =
    rep1sep(Ident ^^ {addtyp(_)} , Comma) ^^ {List() ++ _}

  lazy val ArrayCreateExpression : PackratParser[Array] =
    ArrayCreate ~ Expression ~ Expression ^^ {
      case ac ~ re1 ~ re2 => new Array(re1, re2)
    }

  lazy val EqualityExpression: PackratParser[T] =
    chainl1( RelationalExpression, Equal ^^ {
      case op => (l: T, r: T) => new Eq(l,r)}
    )

  lazy val RelationalExpression: PackratParser[T] =
    chainl1( AdditiveExpression, addRelationalOperator ^^ {
      case op => (l: T, r: T) => new Add(l,r)}
    )

  lazy val AdditiveExpression: PackratParser[T] =
    chainl1( MultiplicativeExpression, (FPlus | Plus | FMinus | Minus) ^^ {
      case FPlus => (l: T, r: T) => new FAdd(l,r)
      case Plus => (l: T, r: T) => new Add(l,r)
      case FMinus => (l: T, r: T) => new FSub(l,r)
      case Minus => (l: T, r: T) => new Sub(l,r)
      }
    )

  lazy val MultiplicativeExpression: PackratParser[T] =
    chainl1( UnaryExpression, (FMUL | FDIV) ^^ {
      case FMUL => (l: T, r: T) => new FMul(l,r)
      case FDIV => (l: T, r: T) => new FDiv(l,r)
      }
    )

  lazy val UnaryExpression: PackratParser[T] =
    FunctionCall |
      FMinus ~ UnaryExpression ^^ { case op~exp => FNeg(exp)} |
      Minus ~ UnaryExpression ^^ { case op~exp => Neg(exp)}

  lazy val FunctionCall: PackratParser[T] =
    WriteArrayElementExpression ~ opt(ActualArgList) ^^ {
      case w~opt => opt match {
        case Some(o) => new App(w, o)
        case None => w
      }
    }

  lazy val ActualArgList : PackratParser[List[T]] =
    rep1sep(WriteArrayElementExpression, WS) ^^ {List() ++ _}

  lazy val WriteArrayElementExpression: PackratParser[T] =
    ReadArrayElementExpression ~ opt(WriteArrayAccess) ^^ {
      case r~opt => opt match {
        case Some(o) => new Put(r, o.head, o.tail.head)
        case None => r
      }
    }

  lazy val WriteArrayAccess: PackratParser[List[T]] =
    Dot ~ LParen ~ Expression ~ RParen ~ LessMinus ~ Expression ^^ {
      case d~lp~e1~rp~lm~e2 => List(e1, e2)
    }

  lazy val ReadArrayElementExpression: PackratParser[T] =
    SimpleExpression ~ opt(ReadArrayAccess) ^^ {
      case s ~ rep => rep match {
        case Some(o) => new Get(s, o)
        case None => s
      }
    }

  lazy val ReadArrayAccess: PackratParser[T] =
    Dot ~ LParen ~ Expression ~ RParen ~ not(LessMinus) ^^ {
      case d~lp~e~rp~n => e
    }

  def parse = {
    //val res = parseAll(LetTupleExpression, "let (hoge1 , hage1  ,hoge2, hage2) = true in false" )
    val res = parseAll(Expression, "let rec bar hoge1  hage1 = true in false" )
    //val res = parseAll(FunctionCall, "true.(false)<-false true.(false)<-false"  )
    //val res = parseAll(Expression, "false.(true).(false)" )
    //val res = parseAll(Expression, "if false then true else false" )
    //val res = parseAll(VariableDeclaration, "let fdsfalse = false in true" )
    //val res = parseAll(RecursiveFunctionDeclaration, "let rec va fdsfalse rue dfas = true and va fdsfalse rue dfas = true in false" )
    println(res)
  }

}


