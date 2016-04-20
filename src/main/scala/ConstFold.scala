package mincaml

import java.util.NoSuchElementException

object ConstFold extends KNormal{

  def main(args:Array[String]) = {

    val parser = new Parser
    val pr = parser.parse("let rec fact n = if n = 0 then 1 else n + fact (n-1) in fact 3")

    val typing = new Typing
    val tp = typing.f(pr.asInstanceOf[typing.T])

    val knormal = new KNormal
    val kn = knormal.f(tp.asInstanceOf[Syntax.T])

    val al = Alpha.f(kn.asInstanceOf[Alpha.T])

    val bt = Beta.f(al.asInstanceOf[Beta.T])

    val as = Assoc.f(bt.asInstanceOf[Assoc.T])

    val il = Inline.f(as.asInstanceOf[Inline.T])

    println(f(il.asInstanceOf[ConstFold.T]))
  }


  def memi(x:Id.T, env:Map[Id.T, T]):scala.Boolean = {
    try {
      env(x) match {
        case Int(_) => true
        case _ => false
      }
    } catch {
      case e:NoSuchElementException => false
    }
  }

  def memf(x:Id.T, env:Map[Id.T, T]):scala.Boolean = {
    try {
      env(x) match {
        case Float(_) => true
        case _ => false
      }
    } catch {
      case e:NoSuchElementException => false
    }
  }

  def memt(x:Id.T, env:Map[Id.T, T]):scala.Boolean = {
    try {
      env(x) match {
        case Tuple(_) => true
        case _ => false
      }
    } catch {
      case e:NoSuchElementException => false
    }
  }

  def findi(x:Id.T, env:Map[Id.T, T]):scala.Int = {
    try {
      env(x) match {
        case Int(i) => i
        case _ => throw new NoSuchElementException
      }
    }
  }

  def findf(x:Id.T, env:Map[Id.T, T]):scala.Double = {
    try {
      env(x) match {
        case Float(d) => d
        case _ => throw new NoSuchElementException
      }
    }
  }

  def findt(x:Id.T, env:Map[Id.T, T]):List[Id.T] = {
    try {
      env(x) match {
        case Tuple(ys) => ys
        case _ => throw new NoSuchElementException
      }
    }
  }

  def foldLeft2(
                 env:T,
                 zs:List[(Id.T, Type.T)],
                 ys:List[Id.T],
                 f1:(T, (Id.T, Type.T), Id.T) => T
               ):T = {

    (zs, ys) match {
      case (z::List(), y::List()) => f1(env, z, y)
      case (z::zt, y::yt) =>
        val env2:T = f1(env, z, y)
        foldLeft2(env2, zt, yt, f1)
      case _ => throw new Exception();
    }
  }

  def g(env:Map[Id.T, T], e:T):T = {

    e match {
      case Var(x) if memi(x, env) => Int(findi(x, env))

      case Neg(x) if memi(x, env) => Int(- findi(x, env))
      case Add(x, y) if memi(x, env) && memi(y, env) => Int(findi(x, env) + findi(y, env))
      case Sub(x, y) if memi(x, env) && memi(y, env) => Int(findi(x, env) - findi(y, env))

      case FNeg(x) if memf(x, env) => Float(- findi(x, env))
      case FAdd(x, y) if memf(x, env) && memf(y, env) => Float(findf(x, env) + findf(y, env))
      case FSub(x, y) if memf(x, env) && memf(y, env) => Float(findf(x, env) - findf(y, env))
      case FMul(x, y) if memf(x, env) && memf(y, env) => Float(findf(x, env) * findf(y, env))
      case FSub(x, y) if memf(x, env) && memf(y, env) => Float(findf(x, env) - findf(y, env))

      case IfEq(x, y, e1, e2) if memi(x, env) && memi(y, env) =>
        if (findi(x, env) == findi(y, env)) g(env, e1)
        else g(env, e2)

      case IfEq(x, y, e1, e2) => IfEq(x, y, g(env, e1), g(env, e2))

      case IfLE(x, y, e1, e2) if memi(x, env) && memi(y, env) =>
        if (findi(x, env) <= findi(y, env)) g(env, e1)
        else g(env, e2)

      case IfLE(x, y, e1, e2) => IfLE(x, y, g(env, e1), g(env, e2))

      case Let((x, t), e1, e2) =>
        val e1p = g(env, e1)
        val e2p = g(env+(x->e1p), e2)
        Let((x, t), e1p, e2p)

      case LetRec(Fundef(name, args, body), e2) =>
        LetRec(
          //bindings.map(bd=>Fundef(bd.name, bd.args, g(env, bd.body))),
          Fundef(name, args, g(env, body)),
          g(env, e2)
        )

      case LetTuple(xts, y, ep) if memt(y, env) =>
        foldLeft2(
          g(env, e),
          xts,
          findt(y, env),
          (envp, xt, z) => Let(xt, Var(z), envp)
        )

      case LetTuple(xts, y, ep) => LetTuple(xts, y, g(env, ep))

      case ep => ep
    }
  }

  def f(e:T):T = {
    g(Map.empty[Id.T, T], e)
  }

}
