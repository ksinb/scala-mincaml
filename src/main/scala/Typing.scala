/**
  * Created by Help Desk on 2016/02/29.
  */
package scala_mincaml

object Typing {
  def main(args: Array[String]) = {
    val res = new Typing()
    println(res.extenv)
    println(res.test)
    println(res.extenv)
  }
}

class Typing extends Syntax {
  var extenv = Map[Id.T, Type.T]()

  def test = {
    //g(Map(), new MBool(false))
    //g(Map(), new MNot(new MBool(false)))
    //g(Map(), new MAdd(new MInt(23), new MInt(46)))
    //g(Map(), new MIf(new MBool(false), new MBool(true), new MInt(42)))
    //g(Map(), new MLet(("fdsfalse", new Type.Var(None)), new MBool(false), new MBool(true)))
    g(Map("vad"->Type.Int()), new MVar("va"))
  }

  def deref_typ = {

  }

  def deref_id_typ = {

  }


  def deref_term = {

  }

  def unify(t:(Type.T, Type.T)) = {
    t  match {
      case (Type.Unit(), Type.Unit()) |
           (Type.Bool(), Type.Bool()) |
           (Type.Int(), Type.Int()) |
           (Type.Float(), Type.Float()) => Unit
      case _ => Unit
    }
  }

  def g(env:Map[Id.T, Type.T], e:T):Type.T = {
    //println(e)

    e match {
      case MUnit() => Type.Unit()
      case MBool(_) => Type.Bool()
      case MInt(_) => Type.Int()
      case MFloat(_) => Type.Float()

      case MNot(e)
        => unify((Type.Bool(), g(env, e))); Type.Bool()
      case MNeg(e)
        => unify((Type.Int(), g(env, e))); Type.Int()

      case MAdd(e1:T, e2:T)
        => unify((Type.Int(), g(env, e1))); unify((Type.Int(), g(env, e2))); Type.Int()
      case MSub(e1:T, e2:T)
        => unify((Type.Int(), g(env, e1))); unify((Type.Int(), g(env, e2))); Type.Int()

      case MFNeg(e)
      => unify((Type.Float(), g(env, e))); Type.Float()

      case MFAdd(e1:T, e2:T)
      => unify((Type.Float(), g(env, e1))); unify((Type.Float(), g(env, e2))); Type.Float()
      case MFSub(e1:T, e2:T)
      => unify((Type.Float(), g(env, e1))); unify((Type.Float(), g(env, e2))); Type.Float()
      case MFMul(e1:T, e2:T)
      => unify((Type.Float(), g(env, e1))); unify((Type.Float(), g(env, e2))); Type.Float()
      case MFDiv(e1:T, e2:T)
      => unify((Type.Float(), g(env, e1))); unify((Type.Float(), g(env, e2))); Type.Float()

      case MEq(e1:T, e2:T)
      => unify((g(env, e1), g(env, e2))); Type.Bool()
      case MNEq(e1:T, e2:T)
      => unify((g(env, e1), g(env, e2))); Type.Bool()
      case MLt(e1:T, e2:T)
      => unify((g(env, e1), g(env, e2))); Type.Bool()
      case MLE(e1:T, e2:T)
      => unify((g(env, e1), g(env, e2))); Type.Bool()

      case MIf(e1:T, e2:T, e3:T) =>
        unify((g(env, e1), Type.Bool()))
        val t2 = g(env, e2); println(t2)
        val t3 = g(env, e3); println(t3)
        val t = unify(t2, t3); println(t)
        t2

      case MLet((x:Id.T, t:Type.T), e1:T, e2:T) =>
        unify(t, g(env, e1))
        g(env+(x->t), e2)

      case MVar(x) =>
        if (env.contains(x)) env(x)
        else if (extenv.contains(x)) extenv(x)
        else {
          val t = Type.gentyp()
          extenv = extenv + (x -> t)
          t
        }
      case _ => Type.Int()
    }
  }

  def f(e:Type.T) = {
    unify((Type.Unit(), Type.Unit()))
  }
}
