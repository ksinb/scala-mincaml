package mincaml

object Typing {
  var extenv = Map[Id.T, Type.T]()
}

class Typing extends Syntax {

  case class Unify(t:(Type.T, Type.T)) extends Exception
  case class Invalid() extends Exception
  var extenv = Map.empty[Id.T, Type.T]

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
      case LetRec(Fundef(name, args, body), e2) =>
        LetRec(
          Fundef(
            deref_id_typ(name),
            args.map((y)=>deref_id_typ(y)),
            deref_term(body)
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

    r2 match {
      case Type.Fun(ts, t) => ts.exists(occur(r1)) || occur(r1)(t)
      case Type.Tuple(ts) => ts.exists(occur(r1))
      case Type.Array(t) => occur(r1)(t)

      case Type.Var(t) if t.isDefined && r1 == t => true
      case Type.Var(t1) =>
        t1 match {
          case Some(t2) => occur(r1)(t2)
          case None => false
        }

      case _ => false
    }
  }

  def unify(t:(Type.T, Type.T)):scala.Unit = {

    t match {
      case (Type.Unit(), Type.Unit())
         | (Type.Bool(), Type.Bool())
         | (Type.Int(), Type.Int())
         | (Type.Float(), Type.Float())
        =>

      case (Type.Fun(t1s, t1), Type.Fun(t2s, t2)) =>
        try {
          (t1s, t2s).zipped.map(unify(_, _))
        } catch {
          case Invalid() => throw Unify(t)
        }
        unify(t1, t2)

      case (Type.Tuple(t1s), Type.Tuple(t2s)) =>
        try {
          (t1s, t2s).zipped.map(unify(_, _))
        } catch {
          case Invalid() => throw Unify(t)
        }
      case (Type.Array(t1), Type.Array(t2)) => unify(t1, t2)
      case (Type.Var(r1), Type.Var(r2)) if r1 == r2 =>

      case (Type.Var(Some(t1)), t2) => unify(t1, t2)
      case (t1, Type.Var(Some(t2))) => unify(t1, t2)

      case (r@Type.Var(r1@None), t2) =>
        if (occur(r1)(t2)) {
          throw Unify(t)
        }
        r.a = Some(t2)

      case (t1, r@Type.Var(r2@None)) =>
        if (occur(r2)(t1)) {
          throw Unify(t)
        }
        r.a = Some(t1)

      case (a, b) => throw Unify(a, b)
    }
  }

  def g(env:Map[Id.T, Type.T])(e:T):Type.T = {

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

        case Add(e1: T, e2: T) =>
          unify(Type.Int(), g(env)(e1)); unify(Type.Int(), g(env)(e2)); Type.Int()

        case Sub(e1, e2) =>
          unify(Type.Int(), g(env)(e1)); unify(Type.Int(), g(env)(e2)); Type.Int()

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

          val g2 = g(env)(e2)
          val g3 = g(env)(e3)
          unify(g2, g3)
          g2

        case Let((x: Id.T, t: Type.T), e1: T, e2: T) =>
          unify(t, g(env)(e1))
          val letval = g(env + (x -> t))(e2)
          letval

        case Var(x) =>
          if (env.contains(x)) env(x)
          else if (extenv.contains(x)) extenv(x)
          else {
            val t = Type.gentyp()
            extenv = extenv + (x -> t)
            t
          }

        case LetRec(Fundef((x, t), args, body), e2) =>
          val ev = env+(x->t)
          unify(t, Type.Fun(args.map(ag=>ag._2), g(ev++args)(body)))
          g(ev)(e2)

        case App(e1, es) =>
          val t1 = Type.gentyp()
          val b = g(env)(e1)
          unify(b, Type.Fun(es.map(g(env)), t1))
          t1

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
      case Unify((t1, t2)) => println("throw typing err"); Type.Unit()
    }
  }

  def f(e:T):T = {
    extenv = Map.empty[Id.T, Type.T]
    g(Map.empty[Id.T, Type.T])(e)
    val rc = deref_term(e)
    rc
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
