package mincaml
import scala.util.parsing.combinator._

object Parser {
  def main(args: Array[String]) = {
   // new Parser().parse()
  }
}

class Parser extends Syntax with RegexParsers with PackratParsers{
  override def skipWhitespace = false

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

  lazy val BOOL: PackratParser[Bool] = (True | False) ^^ {s => Bool(s.toBoolean)}
  lazy val INT: PackratParser[Int] = anySpace ~> regex("""[0-9]+""".r) ^^ {s => Int(s.toInt)}
  lazy val FLOAT: PackratParser[Float] = anySpace ~> regex("""[0-9]+.[0-9]+""".r) ^^ {s => Float(s.toDouble)}

  lazy val File : PackratParser[Any] =
    Expression ~ chainl1(Expression, WS ^^ { _  => (l:Any, r:Any) => (l, r)})

  lazy val Program : PackratParser[Any] =
    repsep(Expression, SemiColon)


  lazy val Expression : PackratParser[T] =
  //repsep(Statement, SemiColon)
    CompaundExpression | EqualityExpression

  lazy val CompaundExpression : PackratParser[T] =
    IfExpression |
    RecursiveFunctionDeclaration |
    VariableDeclaration |
    //TupleExpression |
    LetTupleExpression |
    ArrayCreateExpression

  lazy val IfExpression : PackratParser[If] =
    IF ~ Expression ~ Then ~ Expression ~ Else ~ Expression ^^ {
      case i ~ ex1 ~ t ~ ex2 ~ e ~ ex3 => If(ex1, ex2, ex3)
    }

  lazy val RecursiveFunctionDeclaration : PackratParser[LetRec] =
    LET ~ Rec ~ FUNDEF ~ In ~ Expression ^^ {
      case lt ~ rc ~ fd ~ in ~ exp => LetRec(fd, exp)
    }

  lazy val Fundefs: PackratParser[List[Fundef]] =
    repsep(FUNDEF, And) ^^ {List() ++ _}

  lazy val FUNDEF: PackratParser[Fundef] =
    Ident ~ FormalArgList ~ Equal ~ Expression ^^ {
      case id1 ~ arg ~ eq ~ id2 => new Fundef(addtyp(id1), arg, id2)
    }

  lazy val FormalArgList : PackratParser[List[(Id.T, Type.T)]] =
    repsep(Ident ^^ addtyp, WS) ^^ {List() ++ _}

  lazy val VariableDeclaration : PackratParser[Let] =
    LET ~ Ident ~ Equal ~ Expression ~ In ~ Expression ^^ {
      case l~id~e~exp1~i~exp2 => new Let(addtyp(id), exp1, exp2)
    }

  lazy val Elements : PackratParser[List[T]] =
    rep1sep(Expression, Comma) ^^ {List() ++ _}

  lazy val LetTupleExpression : PackratParser[LetTuple] =
    LET ~ LParen ~ Pat ~ RParen ~ Equal ~ Expression ~ In ~ Expression  ^^ {
      case l~lp~pat~rp~eq~exp1~i~exp2 => new LetTuple(pat, exp1, exp2)
    }

  lazy val Pat : PackratParser[List[(Id.T, Type.T)]] =
    rep1sep(Ident ^^ addtyp , Comma) ^^ {List() ++ _}

  lazy val ArrayCreateExpression : PackratParser[Array] =
    ArrayCreate ~ Expression ~ Expression ^^ {
      case ac ~ re1 ~ re2 => new Array(re1, re2)
    }

  lazy val EqualityExpression: PackratParser[T] =
    chainl1( RelationalExpression, Equal ^^ {
      case op => (l: T, r: T) => new Eq(l,r)
      }
    )

  lazy val RelationalExpression: PackratParser[T] =
    chainl1( AdditiveExpression,
      LessGreater ^^ {case op => (l: T, r: T) => new Not(Eq(l, r))} |
      LessEqual ^^ {case op => (l: T, r: T) => new LE(l, r)} |
      Less ^^ {case op => (l: T, r: T) => new Not(LE(r, l))} |
      GreaterEqual ^^ {case op => (l: T, r: T) => new LE(r, l)} |
      Greater ^^ {case op => (l: T, r: T) => new Not(LE(l, r))}
    )

  lazy val AdditiveExpression: PackratParser[T] =
    chainl1( MultiplicativeExpression,
      FPlus ^^ {case op => (l: T, r: T) => new FAdd(l,r)} |
      Plus ^^ {case op => (l: T, r: T) => new Add(l,r)} |
      FMinus ^^ {case op => (l: T, r: T) => new FSub(l,r)} |
      Minus ^^ {case op => (l: T, r: T) => new Sub(l,r)}
    )

  lazy val MultiplicativeExpression: PackratParser[T] =
    chainl1( UnaryExpression,
      FMUL ^^ {case op => (l: T, r: T) => new FMul(l,r)} |
      FDIV ^^ {case op => (l: T, r: T) => new FMul(l,r)}
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

  lazy val SimpleExpression: PackratParser[T] =
    BOOL |
    //FLOAT |
    INT |
    Ident ^^ {new Var(_)} |
    LParen ~> Expression <~ RParen |
    LParen ~> anySpace <~ RParen ^^ {_ => Unit()}

  lazy val Ident: PackratParser[Id.T] =
    not(Keyword) ~> LocalID ^^ {new Id.T(_)}

  lazy val Keyword: PackratParser[Any] =
    KeywordString ~ not(IdentifierCharacter)

  lazy val KeywordString : PackratParser[String] =
    True | False | IF | Then | Else | LET | In | Rec | ArrayCreate

  lazy val IdentifierCharacter: PackratParser[String] = regex("""[a-zA-Z0-9_]""".r)

  lazy val LocalID: PackratParser[String] =
    anySpace ~> regex("""[a-z_][a-zA-Z0-9_]*""".r)

  lazy val ReadArrayAccess: PackratParser[T] =
    Dot ~ LParen ~ Expression ~ RParen ~ not(LessMinus) ^^ {
      case d~lp~e~rp~n => e
    }


  def parse(src:String) = {
    parseAll(Expression, src) match {
      case Success(r, n) => r
      case Failure(msg, n) => throw new Exception()
    }

    //val res = parseAll(Expression, "let x = (let y = 3 in y + y) in x - x")
    //val res = parseAll(Expression, "let y = 3 in let x = y in x + y")
    //val res = parseAll(Expression, "let rec gcd m n = if m = 0 then n else if m <= n then gcd m (n - m) else gcd n (m - n) in gcd 3 4")
    //val res = parseAll(Expression, "lambda 2.0 1.0")
    //val res = parseAll(Expression, "let a = 1 in let b = 2 in let c = 3 in let d = 4 in  a + b + c -d" )
    //val res = parseAll(Pat, "apple, ho, bananana, kiwi, orange")
    //val res = parseAll(Expression, "let hoge = 1.2 in true")
    //val res = parseAll(Expression, "let (hoge1 , hage1  ,hoge2, hage2) = true in false" )
    //val res = parseAll(Expression, "let rec fname arg1 arg2 = arg1 + arg2 in fname 1.1 2.2" )
    //val res = parseAll(FunctionCall, "true.(false)<-false true.(false)<-false"  )
    //val res = parseAll(Expression, "hoge.(1)" )
    //val res = parseAll(Expression, "if false then 2 else 3.8" )
    //val res = parseAll(VariableDeclaration, "let fdsfalse = false in true" )
    //val res = parseAll(RecursiveFunctionDeclaration, "let rec va fdsfalse rue dfas = true and va fdsfalse rue dfas = true in false" )
  }

}


