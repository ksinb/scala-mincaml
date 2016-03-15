package mincaml

class kNormal {
  sealed abstract class T()
  case class Unit() extends T
  case class Int(int:scala.Int) extends T
  case class Float(double: Double) extends T
  case class Neg(t:Id.T) extends T
  case class Add(a:Id.T, b:Id.T) extends T
  case class Sub() extends T
  case class FNeg() extends T
  case class FAdd() extends T
  case class FSub() extends T
  case class FMul() extends T
  case class FDiv() extends T
  case class IfEq() extends T
  case class IfLE() extends T
  case class Let(t1:(Id.T, Type.T), t2:T, t3:T ) extends T
  case class Var(t:Id.T) extends T
  case class LetRec() extends T
  case class App() extends T
  case class Tuple() extends T
  case class LetTuple() extends T
  case class Get() extends T
  case class Put() extends T
  case class ExtArray() extends T
  case class ExtFunApp() extends T
  case class fundef() extends T

  def insert_let(e0:(T, Type.T), k:Id.T=>(T, Type.T)):(T, Type.T) = {
    e0 match {
      case (Var(x), _)=> k(x)
      case (e, t) =>
        val x = Id.gentmp(t)
        val (e1, t1) = k(x)
        (Let((x, t), e, e1), t1)
    }
  }

  def g(env:Any, t:Syntax.T):(T, Type.T) = {
    t match {
      case Syntax.Unit() => (Unit(), Type.Unit())
      //case Syntax.Bool(b) => Int(1)
      case Syntax.Int(i) => (Int(i), Type.Int())
      case Syntax.Float(d) => (Float(d), Type.Float())
      case Syntax.Not(e) =>
        g(env, Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
      case Syntax.Neg(e) =>
        insert_let(g(env, e), (x: Id.T) => (Neg(x), Type.Int()))
      case Syntax.Add(e1, e2) =>
        insert_let(g(env, e1),
          (x: Id.T) => insert_let(g(env, e2),
            (y:Id.T) => (Add(x, y), Type.Int())))
    }
  }
}

