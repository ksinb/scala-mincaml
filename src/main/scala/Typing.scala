package mincaml

object Typing {

  var extenv = Map[Id.T, Type.T]("lambda"->Type.Int())

  def main(args: Array[String]) = {
    val res = new Typing()
    res.test()
  }
}

class Typing extends Syntax {

  case class Unify(t:(Type.T, Type.T)) extends Exception
  case class Invalid() extends Exception
  var extenv = Map.empty[Id.T, Type.T]

  def test() = {
    /*
    val hoge =  LetRec(
      List(Fundef(("fname",Type.Var(None)),List(("arg1",Type.Var(None)), ("arg2",Type.Var(None))),FAdd(Var("arg1"),Var("arg2")))),
      App(Var("fname"),List(Float(1.1), Float(2.2)))
    )
    */
    val hoge = Let(("a",Type.Var(None)),Int(1),
      Let(("b",Type.Var(None)),Int(2),
        Let(("c",Type.Var(None)),Int(3),
          Let(("d",Type.Var(None)),Int(4),Sub(Add(Add(Var("a"),Var("b")),Var("c")),Var("d"))))))


    //val hoge = Tuple(List(Var("pple"), Int(2), Var("bananana"), Var("kiwi"), Var("orange")))
    /*
    var  hoge =
      LetRec(
        List( Fundef(("bar",Type.Var(None)), List(("hoge1",Type.Float()),("hage1",Type.Float())), Bool(true))),
        Bool(false)
      )

    val hoge =
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
    */


    /*
    val hoge = LetTuple(
      List(("arg1",Type.Var(None)), ("arg2",Type.Var(None)), ("arg3",Type.Var(None)), ("arg4",Type.Var(None))),
      Bool(true),
      Bool(false)
    )
    */
    //val hoge = Get(Var("hoge"),Int(1))
    //var hoge = If(Bool(false),Bool(true),Bool(false))
    //var hoge = Map("vad"->Type.Int()) ++ hoge.fundef.map((fun:Fundef)=>fun.name)
    //var hoge = g( Map("vad"->Type.Int()) )( hoge )
    //val hoge = App(Var("lambda"), List(Float(2.0), Float(1.0)))
    f(hoge)
  }

  def deref_typ(t:Type.T):Type.T = {
      t match  {
        case Type.Fun(t1s, t2) => Type.Fun(t1s.map(deref_typ), deref_typ(t2))
        case Type.Tuple(ts) => Type.Tuple(ts.map(deref_typ))
        case Type.Array(t1) => Type.Array(deref_typ(t1))
        case r@Type.Var(t0) =>
          t0 match{
            case Some(s) => val t1 = deref_typ(s); r.a = Some(t1); t1
            case None => r.a = Some(Type.Int()); Type.Int()
          }
        case _ => t
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
      case If(e1, e2, e3) => If(deref_term(e1), deref_term(e2), deref_term(e3))
      case Let(xt, e1, e2) => Let(deref_id_typ(xt), deref_term(e1), deref_term(e2))
      case LetRec(Fundef(xt, yts, e1), e2) =>
        LetRec(
          //fundefs.map(x => Fundef(deref_id_typ(x.name), x.args.map(deref_id_typ), deref_term(x.body))),
          Fundef(
            deref_id_typ(xt),
            yts.map((y)=>deref_id_typ(y)),
            deref_term(e1)
          ),
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
println("r1",r1, "r2", r2)


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
println("unify", t)

    t match {
      case (Type.Unit(), Type.Unit()) | (Type.Bool(), Type.Bool()) | (Type.Int(), Type.Int()) | (Type.Float(), Type.Float())
        => Unit()

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
      case (Type.Var(r1), Type.Var(r2)) if r1 == r2 =>

      //var, _
      case (r@Type.Var(r1@t1), t2) =>
println("left var")
        t1 match {
          case Some(s) => unify(s, t2)
          case None =>
            if (occur(r1)(t2)) {println("oc found");throw Unify(t)}
            r.a = Some(t2)
        }
      //_, var
      case (t1, r@Type.Var(r2@t2)) =>
println("right var")
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
println("[function g]", e)

    try {
      e match {
        case Unit() => Type.Unit()
        case Bool(_) => Type.Bool()
        case Int(_) => Type.Int()
        case Float(_) => Type.Float()

        case Not(e1)
        => unify(Type.Bool(), g(env)(e1)); Type.Bool()
        case Neg(e1)
        => unify(Type.Int(), g(env)(e1)); Type.Int()

        case Add(e1: T, e2: T)
        => unify(Type.Int(), g(env)(e1)); unify(Type.Int(), g(env)(e2)); Type.Int()

        case Sub(e1, e2)
        => unify(Type.Int(), g(env)(e1)); unify(Type.Int(), g(env)(e2)); Type.Int()

        case FNeg(e1)
        => unify(Type.Float(), g(env)(e1)); Type.Float()

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
          val t2 = g(env)(e2)
          val t3 = g(env)(e3)
          unify(t2, t3)
          t2

        case Let((x: Id.T, t: Type.T), e1: T, e2: T) =>
          unify(t, g(env)(e1))
          println("unify end", e1, e2)
          val letval = g(env + (x -> t))(e2)
          println("letval", e1, e2, letval)
          letval

        case Var(x) =>
          if (env.contains(x)) {println("env found", env);env(x)}
          else if (extenv.contains(x)) {println("extenv found");extenv(x)}
          else {
            val t = Type.gentyp()
            extenv = extenv + (x -> t)
            println(extenv)
            t
          }

        case LetRec(Fundef((x, t), yts, e1), e2) =>
          val ev = env + (x->t)
          unify(
            t,
            Type.Fun(yts.map((y) => y._2), g(ev++yts)(e1))
          )
          g(ev)(e2)

          /*
          fundefs.foreach(
            fd => unify(
              fd.name._2,
              Type.Fun(fd.args.map(ag => ag._2),
              g(ev ++ fd.args)(fd.body))
            )
          )
          */
          g(ev)(e2)

        case App(e1, es) =>
          println("App found", e1, es)
          val t = Type.gentyp()
          val b = g(env)(e1)
          println("b end", b)
          unify(b, Type.Fun(es.map(g(env)), t))
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

    val result = g(Map.empty[Id.T, Type.T])(e)
    println("result")
    println(result, extenv)
    println(deref_term(e))

/*
    val ddd = deref_typ(result)
    println("deref:", ddd)

    ddd match {
      case Type.Unit() => Unit()
      case _ => throw new Exception()
    }
*/

    /*
    try {
      unify(Type.Unit(), g(Map.empty[Id.T, Type.T])(e))
    } catch {
      case Unify(_) => println("top level does not have type unit")
    }
    extenv = extenv.map(deref_typ)
    deref_term(e)
    */
  }
}
