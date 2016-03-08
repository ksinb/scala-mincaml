/**
  * Created by Help Desk on 2016/02/29.
  */
package scala_mincaml

object Typing {
  def main(args: Array[String]) = {
    val res = new Typing()
    //println(res.extenv)
    println(res.test)
    //println(res.extenv)
  }
}

class Typing extends Syntax {

  case class Unify(t:(Type.T, Type.T)) extends Exception
  case class Invalid() extends Exception
  var extenv = Map.empty[Id.T, Type.T]

  def test = {
    //g(Map())(Bool(false))


    //g(Map())(If(new MBool(false), new Bool(true), new Int(42)))
    //g(Map())(Let(("x", Type.Var(None)), Int(1), Add(Int(1), Int(3))))
    //g(Map("vad"->Type.Int()))(MVar("va"))
/*
    var  hoge =
      LetRec(
        List( Fundef(("bar",Type.Var(None)), List(("hoge1",Type.Float()),("hage1",Type.Float())), Bool(true))),
        Bool(false)
      )
*/
    var hoge =
      LetRec(
        List(
          Fundef(
            ("myfunc",Type.Var(None)),
            List(("ag1",Type.Var(None)), ("ag2",Type.Var(None))),
            Int(2)
          )
        ),
        Bool(false)
      )

    //var hoge = If(Bool(false),Bool(true),Bool(false))
    //var hog = Map("vad"->Type.Int()) ++ hoge.fundef.map((fun:Fundef)=>fun.name)
    //var hog = g( Map("vad"->Type.Int()) )( hoge )

    println(hoge)
    f(hoge)
    println("hoge")
    println(hoge)
  }

  def deref_typ(t:Type.T):Type.T = {
      t match  {
        case Type.Fun(t1s, t2) => Type.Fun(t1s.map(deref_typ), deref_typ(t2))
        case Type.Tuple(ts) => Type.Tuple(ts.map(deref_typ))
        case Type.Array(t) => Type.Array(deref_typ(t))
        case r@Type.Var(t) =>
          t match{
            case Some(s) => val t1 = deref_typ(s); r.a = Some(t1); t1
            case None => r.a = Some(Type.Int()); Type.Int()
          }
        case t => t

      }
  }

  def deref_id_typ(x:(Id.T, Type.T)):(Id.T, Type.T) = (x._1, deref_typ(x._2))

  def deref_term(t:T):T = {
    t match {
      case Not(e) => Not(deref_term(e))
      case Neg(e) => Neg(deref_term(e))
      case Add(e1, e2) => Add(deref_term(e1), deref_term(e2))
      case Sub(e1, e2) => Sub(deref_term(e1), deref_term(e2))
      case Eq(e1, e2) => Eq(deref_term(e1), deref_term(e2))
      case NEq(e1, e2) => NEq(deref_term(e1), deref_term(e2))
      case Lt(e1, e2) => Lt(deref_term(e1), deref_term(e2))
      case LE(e1, e2) => LE(deref_term(e1), deref_term(e2))
      case FNeg(e) => FNeg(deref_term(e))
      case FAdd(e1, e2) => FAdd(deref_term(e1), deref_term(e2))
      case FSub(e1, e2) => FSub(deref_term(e1), deref_term(e2))
      case FMul(e1, e2) => FMul(deref_term(e1), deref_term(e2))
      case FDiv(e1, e2) => FDiv(deref_term(e1), deref_term(e2))
      case If(e1, e2, e3) => If(deref_term(e1), deref_term(e2), deref_term((e3)))
      case Let(xt, e1, e2) => Let(deref_id_typ(xt), deref_term(e1), deref_term(e2))
      case LetRec(fundefs, e2) =>
        LetRec(
          fundefs.map(x => Fundef(deref_id_typ(x.name), x.args.map(deref_id_typ), deref_term(x.body))),
          deref_term(e2)
        )
      case App(e, es) => App(deref_term(e), es.map(deref_term))
      case Tuple(es) => Tuple(es.map(deref_term))
      case LetTuple(xts, e1, e2) => LetTuple(xts.map(deref_id_typ), deref_term(e1), deref_term(e2))
      case Array(e1, e2) => Array(deref_term(e1), deref_term(e2))
      case Get(e1, e2) => Get(deref_term(e1), deref_term(e2))
      case Put(e1, e2, e3) => Put(deref_term(e1), deref_term(e2), deref_term(e3))
      case e => e
    }
  }

  def occur(r1:Option[Type.T])(r2:Type.T):Boolean = {
    println("r1")
    println(r1)
    println("r2")
    println(r2)
    r2 match {
      case Type.Fun(ts, t) => ts.exists(occur(r1)) || occur(r1)(t)
      case Type.Tuple(ts) => ts.exists(occur(r1))
      case Type.Array(t) => occur(r1)(t)

      case Type.Var(t) if r1 == t => true
      case Type.Var(t1) =>
        t1 match {
          case Some(t2) => occur(r1)(t2)
          case None => false
        }

      case _ => false
    }
  }

  def unify(t:(Type.T, Type.T)):scala.Unit = {
    println("unify")
println(t)
    t  match {
      case (Type.Unit(), Type.Unit()) |
           (Type.Bool(), Type.Bool()) |
           (Type.Int(), Type.Int()) |
           (Type.Float(), Type.Float())
        => ()

      //fun, fun
      case (Type.Fun(t1s, t1), Type.Fun(t2s, t2)) =>
        try {
          (t1s, t2s).zipped.map(unify(_, _))
        } catch {
          case Invalid() => throw Unify(t)
        }
        unify(t1, t2)

      //tuple, tuple
      case (Type.Tuple(t1s), Type.Tuple(t2s)) =>
        try {
          (t1s, t2s).zipped.map(unify(_, _))
        } catch {
          case Invalid() => throw Unify(t)
        }
      case (Type.Array(t1), Type.Array(t2)) => unify(t1, t2)
      case (Type.Var(r1), Type.Var(r2)) if r1 == r2 => ()

      //var, _
      case (r@Type.Var(r1@t1), t2) =>
        println("varr")
        t1 match {
          case Some(s) => unify(s, t2)
          case None =>
            if (occur(r1)(t2)) {println("oc");throw Unify(t)}
            println("noc")
            r.a = Some(t2)
        }
      //_, var
      case (t1, r@Type.Var(r2@t2)) =>
        t2 match {
          case Some(s) => unify(t1, s)
          case None =>
            if (occur(r2)(t1)) throw Unify(t)

            r.a = Some(t1)
        }

      //_, _
      case (a, b) => println("throw unify");throw Unify(a, b)
    }
  }

  def g(env:Map[Id.T, Type.T])(e:T):Type.T = {

    try {
      e match {
        case Unit() => Type.Unit()
        case Bool(_) => Type.Bool()
        case Int(_) => Type.Int()
        case Float(_) => Type.Float()

        case Not(e)
        => unify(Type.Bool(), g(env)(e)); Type.Bool()
        case Neg(e)
        => unify(Type.Int(), g(env)(e)); Type.Int()

        case Add(e1: T, e2: T)
        => unify(Type.Int(), g(env)(e1)); unify(Type.Int(), g(env)(e2)); Type.Int()

        case Sub(e1, e2)
        => unify(Type.Int(), g(env)(e1)); unify(Type.Int(), g(env)(e2)); Type.Int()

        case FNeg(e)
        => unify(Type.Float(), g(env)(e)); Type.Float()

        case FAdd(e1: T, e2: T)
        => unify(Type.Float(), g(env)(e1)); unify(Type.Float(), g(env)(e2)); Type.Float()
        case FSub(e1: T, e2: T)
        => unify(Type.Float(), g(env)(e1)); unify(Type.Float(), g(env)(e2)); Type.Float()
        case FMul(e1: T, e2: T)
        => unify(Type.Float(), g(env)(e1)); unify(Type.Float(), g(env)(e2)); Type.Float()
        case FDiv(e1: T, e2: T)
        => unify(Type.Float(), g(env)(e1)); unify(Type.Float(), g(env)(e2)); Type.Float()

        case Eq(e1: T, e2: T)
        => unify(g(env)(e1), g(env)(e2)); Type.Bool()
        case NEq(e1: T, e2: T)
        => unify(g(env)(e1), g(env)(e2)); Type.Bool()
        case Lt(e1: T, e2: T)
        => unify(g(env)(e1), g(env)(e2)); Type.Bool()
        case LE(e1: T, e2: T)
        => unify(g(env)(e1), g(env)(e2)); Type.Bool()

        case If(e1: T, e2: T, e3: T) =>
          unify(g(env)(e1), Type.Bool())
          val t2 = g(env)(e2);
          val t3 = g(env)(e3);
          unify(t2, t3);
          t2

        case Let((x: Id.T, t: Type.T), e1: T, e2: T) =>
          unify(t, g(env)(e1))
          g(env + (x -> t))(e2)

        case Var(x) =>
          if (env.contains(x)) env(x)
          else if (extenv.contains(x)) extenv(x)
          else {
            val t = Type.gentyp()
            extenv = extenv + (x -> t)
            t
          }

        case LetRec(fundefs, e2) =>
println("letrec")

          val ev = env ++ fundefs.map((fd: Fundef) => fd.name)
          println(ev)
          fundefs.map(
            fd => unify(fd.name._2, Type.Fun(fd.args.map(ag => ag._2), g(ev ++ fd.args)(fd.body)))
          )
          println("letrec")
          g(ev)(e2)

        case App(e: T, es) =>
          val t = Type.gentyp()
          unify(g(env)(e), Type.Fun(es.map(g(env)), t))
          t

        case Tuple(es) =>
          Type.Tuple(es.map(g(env)))

        case LetTuple(xts, e1, e2) =>
          unify(Type.Tuple(xts.map(xt => xt._2)), g(env)(e1))
          g(env ++ xts)(e2)

        case Array(e1, e2) =>
          unify(g(env)(e1), Type.Int())
          Type.Array(g(env)(e2))

        case Get(e1, e2) =>
          val t = Type.gentyp()
          unify(Type.Array(t), g(env)(e1))
          unify(Type.Int(), g(env)(e2))
          t

        case Put(e1, e2, e3) =>
          val t = g(env)(e3)
          unify(Type.Array(t), g(env)(e1))
          unify(Type.Int(), g(env)(e2))
          Type.Unit()
      }
    } catch {
      case Unify((t1, t2)) => println(t1, t2); println("throw err");Type.Unit()//throw new Error

    }
  }

  def f(e:T) = {
    extenv = Map.empty[Id.T, Type.T]
    println(deref_typ(g(extenv)(e)))

    /*
    try {
      unify(Type.Unit(), g(Map.empty[Id.T, Type.T])(e))
    } catch {
      case Unify(_) => println("top level does not have type unit")
    }
    extenv = extenv.map()
    deref_term(e)
    */
  }
}
