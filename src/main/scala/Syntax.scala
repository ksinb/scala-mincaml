package mincaml

object Syntax extends Syntax

class Syntax {
  sealed abstract class T()
  case class Unit() extends T
  case class Bool(t: Boolean) extends T
  case class Int(t: scala.Int) extends T
  case class Float(t: Double) extends T
  case class Not(t: T) extends T
  case class Neg(t: T) extends T
  case class FNeg(t: T) extends T
  case class Add(t: T, u: T) extends T
  case class FAdd(t: T, u: T) extends T
  case class Sub(t: T, u: T) extends T
  case class FSub(t: T, u: T) extends T
  case class FMul(t: T, u: T) extends T
  case class FDiv(t: T, u: T) extends T

  case class Eq(t: T, u: T) extends T
  case class NEq(t: T, u: T) extends T
  case class Lt(t: T, u: T) extends T
  case class LE(t: T, u: T) extends T

  case class If(t: T, u: T, v: T) extends T
  case class Let(a: (Id.T, Type.T), c: T, b: T) extends T
  case class Var(b: Id.T) extends T

  case class LetRec(a:List[Fundef], t:T) extends T
  case class App(t: T, l: List[T]) extends T
  case class Tuple(t: List[T]) extends T
  case class LetTuple(a: List[(Id.T, Type.T)], b: T, c: T) extends T
  case class Array(left: T, right: T) extends T
  case class Get(left: T, right: T) extends T
  case class Put(left: T, center: T, right: T) extends T

  case class Fundef(name:(Id.T, Type.T), args:List[(Id.T, Type.T)], body:T)

  def addtyp(x: Id.T): (Id.T, Type.T) = {
    (x, Type.gentyp())
  }
}

