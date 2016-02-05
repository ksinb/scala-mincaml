/**
  * Created by Help Desk on 2016/02/05.
  */


  class Ast {

    case class Unit() extends Ast

    case class Bool(value: String) extends Ast

    case class Int() extends Ast

    case class Float() extends Ast

    case class Neg() extends Ast

    case class Add() extends Ast

    case class Sub() extends Ast

    case class FNeg() extends Ast

    case class FAdd() extends Ast

    case class FSub() extends Ast

    case class FMul() extends Ast

    case class FDiv() extends Ast

    case class Eq() extends Ast

    case class LE() extends Ast

    case class If() extends Ast

    case class Let() extends Ast

    case class Var() extends Ast

    case class LetRec() extends Ast

    case class App() extends Ast

    case class Tuple() extends Ast

    case class LetTuple() extends Ast

    case class Array() extends Ast

    case class Get() extends Ast

    case class Put() extends Ast

    case class Fundef() extends Ast

    case class Not() extends Ast

  }