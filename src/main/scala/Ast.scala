/**
  * Created by Help Desk on 2016/02/05.
  */
abstract class Ast
case class Unit(value:String) extends Ast
case class Bool(value:String) extends Ast
case class Int(value:String) extends Ast
case class Float(value:String) extends Ast
case class Neg(value:String) extends Ast
case class Add(value:String) extends Ast
case class Sub(value:String) extends Ast
case class FNeg(value:String) extends Ast
case class FAdd(value:String) extends Ast
case class FSub(value:String) extends Ast
case class FMul(value:String) extends Ast
case class FDiv(value:String) extends Ast
case class Eq(value:String) extends Ast
case class LE(value:String) extends Ast
case class If(value:String) extends Ast
case class Let(value:String) extends Ast
case class Var(value:String) extends Ast
case class LetRec(value:String) extends Ast
case class App(value:String) extends Ast
case class Tuple(value:String) extends Ast
case class LetTuple(value:String) extends Ast
case class Array(value:String) extends Ast
case class Get(value:String) extends Ast
case class Put(value:String) extends Ast
case class Fundef(value:String) extends Ast
case class Not(value:String) extends Ast
