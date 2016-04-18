package mincaml

object Beta extends KNormal{

  def main(args:Array[String]) = {
    /*
    val hoge = Syntax.Let(("a", Type.Int()), Syntax.Int(1),
    Syntax.Let(("a", Type.Int()), Syntax.Int(2),
      Syntax.Let(("b", Type.Int()), Syntax.Int(3),
        Syntax.Let(("b", Type.Int()), Syntax.Int(4), Syntax.Sub(Syntax.Add(Syntax.Add(Syntax.Var("a"), Syntax.Var("a")), Syntax.Var("b")), Syntax.Var("b"))))))
*/
    val hoge = Syntax.Let(("y",Type.Int()),Syntax.Int(3),Syntax.Let(("x",Type.Int()),Syntax.Var("y"),Syntax.Add(Syntax.Var("x"),Syntax.Var("y"))))

    val kn = new KNormal
    val d = kn.f(hoge)
    println(d)

    val al = Alpha.f(d.asInstanceOf[Alpha.T])
    println(al)

    val bt = Beta.f(al.asInstanceOf[T])
    println(bt)
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
      case IfLE(x, y, e1, e2) => IfEq(find(x, env), find(y, env), g(env, e1), g(env, e2))

      case Let((x, t), e1, e2) =>
        g(env, e1) match {
          case Var(y) => println("beta-reducing"); g(env+(x->y), e2)
          case e1p =>
            val e2p = g(env, e2)
            Let((x, t), e1p, e2p)
        }

      case LetRec(fundefs, e) =>
        val fds = fundefs.map(fd=>Fundef(fd.name, fd.args, g(env, fd.body)))
        LetRec(fds, g(env, e))

      case Var(x) =>
        Var(find(x, env))

      case Tuple(xs) =>
        Tuple(xs.map(x=>find(x, env)))

      case LetTuple(xts, y, ep) =>
        LetTuple(xts, find(y, env), g(env, ep))

      case Get(x, y) =>
        Get(find(x, env), find(y, env))

      case Put(x, y, z) =>
        Put(find(x, env), find(y, env), find(z, env))

      case App(g, xs) =>
        App(find(g, env), xs.map(x=>find(x, env)))

      case ExtArray(x) => ExtArray(x)

      case ExtFunApp(x, ys) => ExtFunApp(x, ys.map(y=>find(y, env)))
    }
  }

  def f(e:T):T = {
    g(Map.empty[Id.T, Id.T], e)
  }
}
