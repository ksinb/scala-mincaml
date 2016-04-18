package mincaml

object KNormal {
  def main(args: Array[String]) = {

    val env = Map[Id.T, Type.T]("lambda"->Type.Int())
    //val hoge = Syntax.Add(Syntax.Float(123.0),Syntax.Float(456.0))
    //val hoge = Syntax.Not(Syntax.Bool(false))
    //val hoge = Syntax.Var("lambda")
    //val hoge = Syntax.App(Syntax.Var("lambda"),List(Syntax.Float(2.0), Syntax.Float(1.0)))
    val hoge = Syntax.Let(("a",Type.Int()),Syntax.Int(1),
      Syntax.Let(("b",Type.Int()),Syntax.Int(2),
        Syntax.Let(("c",Type.Int()),Syntax.Int(3),
          Syntax.Let(("d",Type.Int()),Syntax.Int(4),Syntax.Sub(Syntax.Add(Syntax.Add(Syntax.Var("a"),Syntax.Var("b")),Syntax.Var("c")),Syntax.Var("d"))))))

    val kn = new KNormal
    println(kn.f(hoge))
  }
}

class KNormal {
  sealed abstract class T()
  case class Unit() extends T
  case class Int(int:scala.Int) extends T
  case class Float(double: Double) extends T
  case class Neg(t:Id.T) extends T
  case class Add(a:Id.T, b:Id.T) extends T
  case class Sub(a:Id.T, b:Id.T) extends T
  case class FNeg(t:Id.T) extends T
  case class FAdd(a:Id.T, b:Id.T) extends T
  case class FSub(a:Id.T, b:Id.T) extends T
  case class FMul(a:Id.T, b:Id.T) extends T
  case class FDiv(a:Id.T, b:Id.T) extends T
  case class IfEq(a:Id.T, b:Id.T, c:T, d:T) extends T
  case class IfLE(a:Id.T, b:Id.T, c:T, d:T) extends T
  case class Let(t1:(Id.T, Type.T), t2:T, t3:T) extends T
  case class Var(t:Id.T) extends T
  case class LetRec(a:List[Fundef], t:T) extends T
  case class App(t:Id.T, ts:List[Id.T]) extends T
  case class Tuple(ts:List[Id.T]) extends T
  case class LetTuple(t:List[(Id.T, Type.T)], u:Id.T, v:T) extends T
  case class Get(t:Id.T, u:Id.T) extends T
  case class Put(s:Id.T, t:Id.T, u:Id.T) extends T
  case class ExtArray(t:Id.T) extends T
  case class ExtFunApp(t:Id.T, ts:List[Id.T]) extends T
  case class Fundef(name:(Id.T, Type.T), args:List[(Id.T, Type.T)], body:T) extends T

  def f(e:Syntax.T):T = {
    val result = g(Map[Id.T, Type.T](), e)
    result._1
  }

  def insert_let(e0:(T, Type.T), k:Id.T=>(T, Type.T)):(T, Type.T) = {
    e0 match {
      case (Var(x), _)=> k(x)
      case (e, t) =>
        val x = Id.gentmp(t)
        val (e1, t1) = k(x)
        (Let((x, t), e, e1), t1)
    }
  }

  def g(env:Map[Id.T, Type.T], t0:Syntax.T):(T, Type.T) = {
    t0 match {
      case Syntax.Unit() => (Unit(), Type.Unit())
      //case Syntax.Bool(b) => Int(1)
      case Syntax.Int(i) => (Int(i), Type.Int())
      case Syntax.Float(d) => (Float(d), Type.Float())
      case Syntax.Not(e)
      => g(env, Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
      case Syntax.Neg(e)
      => insert_let(g(env, e), (x: Id.T) => (Neg(x), Type.Int()))
      case Syntax.Add(e1, e2) =>
        insert_let(g(env, e1),
          (x: Id.T) => insert_let(g(env, e2),
            (y: Id.T) => (Add(x, y), Type.Int())))
      case Syntax.Sub(e1, e2) =>
        insert_let(g(env, e1),
          (x: Id.T) => insert_let(g(env, e2),
            (y: Id.T) => (Sub(x, y), Type.Int())))

      case Syntax.FNeg(e)
      => insert_let(g(env, e), (x: Id.T) => (FNeg(x), Type.Float()))
      case Syntax.FAdd(e1, e2) =>
        insert_let(g(env, e1),
          (x: Id.T) => insert_let(g(env, e2),
            (y: Id.T) => (FAdd(x, y), Type.Float())))
      case Syntax.FSub(e1, e2) =>
        insert_let(g(env, e1),
          (x: Id.T) => insert_let(g(env, e2),
            (y: Id.T) => (FSub(x, y), Type.Float())))
      case Syntax.FMul(e1, e2) =>
        insert_let(g(env, e1),
          (x: Id.T) => insert_let(g(env, e2),
            (y: Id.T) => (FMul(x, y), Type.Float())))
      case Syntax.FDiv(e1, e2) =>
        insert_let(g(env, e1),
          (x: Id.T) => insert_let(g(env, e2),
            (y: Id.T) => (FDiv(x, y), Type.Float())))

      case cmp@(Syntax.Eq(_, _) | Syntax.NEq(_, _) | Syntax.Lt(_, _) | Syntax.LE(_, _))
      => g(env, Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))

      case Syntax.If(Syntax.Not(e1), e2, e3)
      => g(env, Syntax.If(e1, e3, e2))

      case Syntax.If(Syntax.Eq(e1, e2), e3, e4) =>
        insert_let(g(env, e1),
          (x: Id.T) => insert_let(g(env, e2),
            (y: Id.T) => {
              val (e3p, t3) = g(env, e3)
              val (e4p, _) = g(env, e4)
              (IfEq(x, y, e3p, e4p), t3)
            }
          )
        )

      case Syntax.If(Syntax.NEq(e1, e2), e3, e4)
      => g(env, Syntax.If(Syntax.Eq(e1, e2), e4, e3))

      case Syntax.If(Syntax.Lt(e1, e2), e3, e4)
      => g(env, Syntax.If(Syntax.LE(e2, e1), e4, e3))

      case Syntax.If(Syntax.LE(e1, e2), e3, e4) =>
        insert_let(g(env, e2),
          (x: Id.T) => insert_let(g(env, e2),
            (y: Id.T) => {
              val (e3p, t3) = g(env, e3)
              val (e4p, _) = g(env, e4)
              (IfLE(x, y, e3p, e4p), t3)
            }
          )
        )

      case Syntax.If(e1, e2, e3)
      => g(env, Syntax.If(Syntax.NEq(e1, Syntax.Bool(false)), e2, e3))

      case Syntax.Let((x, t), e1, e2) =>
        val (e1p, _) = g(env, e1)
        val (e2p, t2) = g(env + (x -> t), e2)
        (Let((x, t), e1p, e2p), t2)

      case Syntax.Var(x) if env.contains(x)
      => (Var(x), env(x))

      case Syntax.Var(x) =>
        Typing.extenv(x) match {
          case t@Type.Array(_) => (ExtArray(x), t)
          case _ => throw new Exception();
        }
/*
      case Syntax.LetRec(Syntax.Fundef((x, t), yts, e1), e2) =>
        val env1 = env + (x->t)
        val (e2p, t2) = g(env1, e2)
        val (e1p, _) = g(env1 ++ yts, e1)
        (LetRec(
          Fundef((x,t), yts, e1p),
          e2p),
          t2
        )
*/
      case Syntax.LetRec(fundefs, e2) =>
        val env1 = env ++ fundefs.map(fd=>fd.name)
        val (e2p, t2) = g(env1, e2)
        (
          LetRec(
            fundefs.map(fd => {
              val (e1p, _) = g(env1 ++ fd.args, fd.body)
              Fundef(fd.name, fd.args, e1p)
            }),
            e2p),
          t2
        )

      case Syntax.App(Syntax.Var(f), e1s) if !env.contains(f) =>
        Typing.extenv(f) match {
          case Type.Fun(_, t) =>
            def bind(xs: List[Id.T], arg: List[Syntax.T]): (T, Type.T) = {
              arg match {
                case List() => (ExtFunApp(f, xs), t)
                case e2 :: e2s => insert_let(g(env, e2),
                  (x: Id.T) => bind(xs ::: List(x), e2s))
              }
            }
            bind(List(), e1s)
          case _ => throw new Exception()
        }

      case Syntax.App(e1, e1s) =>
        g(env, e1) match {
          case et1@(e1p, Type.Fun(_, t)) =>
            insert_let(
              et1,
              (f: Id.T) => {
                def bind(xs: List[Id.T], arg: List[Syntax.T]): (T, Type.T) = {
                  arg match {
                    case List() => (App(f, xs), t)
                    case e2 :: e2s => insert_let(g(env, e2),
                      (x: Id.T) => bind(xs ::: List(x), e2s))
                  }
                }
                bind(List(), e1s)
              }
            )
          case _ => throw new Exception()
        }

      case Syntax.Tuple(es) =>
        def bind(xs: List[Id.T], ts: List[Type.T], arg: List[Syntax.T]): (T, Type.T) = {
          arg match {
            case List() => (Tuple(xs), Type.Tuple(ts))
            case e :: es1 =>
              val et@(_, t) = g(env, e)
              insert_let(
                et,
                (x: Id.T) => {
                  bind(xs ::: List(x), ts ::: List(t), es1)
                }
              )
          }
        }
        bind(List(), List(), es)

      case Syntax.LetTuple(xts, e1, e2) =>
        insert_let(
          g(env, e1),
          (y: Id.T) => {
            val (e2p, t2) = g(env ++ xts, e2)
            (LetTuple(xts, y, e2p), t2)
          }
        )

      case Syntax.Array(e1, e2) =>
        insert_let(
          g(env, e1),
          (x:Id.T) => {
            val et2@(_, t2) = g(env, e2)
            insert_let(
              et2,
              (y:Id.T) => {
                val l = t2 match {
                  case Type.Float() => "create_float_array"
                  case _ => "create_array"
                }
                (ExtFunApp(l, List(x, y)), Type.Array(t2))
              }
            )
          }
        )

      case Syntax.Get(e1, e2) =>
        g(env, e1) match {
          case et1@(e1p, Type.Array(t)) =>
            insert_let(
              et1,
              (x: Id.T) => {
                insert_let(
                  g(env, e2),
                  (y: Id.T) => (Get(x, y), t)
                )
              }
            )
          case _ =>  throw new Exception()
        }

      case Syntax.Put(e1, e2, e3) =>
        insert_let(g(env, e1),
          (x: Id.T) => {
            insert_let(g(env, e2),
              (y: Id.T) => {
                insert_let(g(env, e3),
                  (z: Id.T) => {(Put(x, y, z), Type.Unit())}
                )
              }
            )
          }
        )
    }
  }
}

