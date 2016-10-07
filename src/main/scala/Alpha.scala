package mincaml

object Alpha extends KNormal{
/*
  def main(args:Array[String]) = {
    val hoge = Syntax.Let(("a", Type.Int()), Syntax.Int(1),
      Syntax.Let(("a", Type.Int()), Syntax.Int(2),
        Syntax.Let(("b", Type.Int()), Syntax.Int(3),
          Syntax.Let(("b", Type.Int()), Syntax.Int(4), Syntax.Sub(Syntax.Add(Syntax.Add(Syntax.Var("a"), Syntax.Var("a")), Syntax.Var("b")), Syntax.Var("b"))))))
    val kn = new KNormal
    val d = kn.f(hoge)
    println(d)
    println(f(d.asInstanceOf[T]))
  }
*/
  def apply(e:KNormal.T):KNormal.T = {
    g(Map[Id.T, Id.T](), e.asInstanceOf[T]).asInstanceOf[KNormal.T]
  }

  def f(e:T):T = {
    g(Map.empty[Id.T, Id.T], e)
  }

  def find(x:Id.T, env:Map[Id.T, Id.T]) = {
    env get x match {
      case Some(s) => s
      case None => x
    }
  }

  def g(env:Map[Id.T, Id.T], e:T):T = {
    e match {
      case Unit() => Unit()
      case Int(i) => Int(i)
      case Float(d) => Float(d)
      case Neg(x) => Neg(find(x, env))
      case Add(x, y) => Add(find(x, env), find(y, env))
      case Sub(x, y) => Sub(find(x, env), find(y, env))
      case FNeg(x) => FNeg(find(x, env))
      case FAdd(x, y) => FAdd(find(x, env), find(y, env))
      case FSub(x, y) => FSub(find(x, env), find(y, env))
      case FMul(x, y) => FMul(find(x, env), find(y, env))
      case FDiv(x, y) => FDiv(find(x, env), find(y, env))

      case IfEq(x, y, e1, e2) => IfEq(find(x, env), find(y, env), g(env, e1), g(env, e2))
      case IfLE(x, y, e1, e2) => IfLE(find(x, env), find(y, env), g(env, e1), g(env, e2))
      case Let((x, t), e1, e2) =>
        val xp = Id.genid(x)
        Let((xp, t), g(env, e1), g(env+(x->xp), e2))

      case Var(x) => Var(find(x, env))

      case LetRec(Fundef((x,t), yts, e1), e2) =>
        val env1 = env + (x->Id.genid(x))
        val envp = yts.foldLeft(env1){ case (ep, (k, _)) => ep+(k->Id.genid(k)) }
        LetRec(
          Fundef(
            (find(x, env1), t),
            yts.map{ case (y, t1) => (find(y, envp), t1) },
            g(envp, e1)
          ),
          g(env1, e2)
        )

      case App(x, ys) => App(find(x, env), ys.map((y)=>find(y, env)))
      case Tuple(xs) => Tuple(xs.map((x)=>find(x, env)))

      case LetTuple(xts, y, e2) =>
        val envp = xts.foldLeft(env){ case (e1, (k, _)) => e1+(k->Id.genid(k)) }
        LetTuple(
          xts.map{ case (x, t) => (find(x, envp), t) },
          find(y, env),
          g(envp, e2)
        )

      case Get(x, y) => Get(find(x, env), find(y, env))
      case Put(x, y, z) => Put(find(x, env), find(y, env), find(z, env))
      case ExtArray(x) => ExtArray(x)
      case ExtFunApp(x, ys) => ExtFunApp(x, ys.map((y)=>find(y, env)))
    }
  }
}
