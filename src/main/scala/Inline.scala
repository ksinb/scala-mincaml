package mincaml

object Inline extends KNormal{

  var threshold = 0

  def size(e:T):scala.Int = {
    e match {
      case IfEq(_, _, e1, e2) =>  1 + size(e1) + size(e2)
      case IfLE(_, _, e1, e2) =>  1 + size(e1) + size(e2)
      case Let(_, e1, e2) => 1 + size(e1) + size(e2)
      case LetRec(Fundef(name, args, body), e2) => 1 + size(body) + size(e2)
      case LetTuple(_, _, ep) => 1 + size(ep)
      case _ => 1
    }
  }

  def foldLeft2(
                 env:Map[Id.T, Id.T],
                 zs:List[(Id.T, Type.T)],
                 ys:List[Id.T],
                 f:(Map[Id.T, Id.T], (Id.T, Type.T), Id.T) => Map[Id.T, Id.T]
               ):Map[Id.T, Id.T] = {

    (zs, ys) match {
      case (z :: List(), y :: List()) => f(env, z, y)
      case (z :: zt, y :: yt) =>
        val env2:Map[Id.T, Id.T] = f(env, z, y)
        foldLeft2(env2, zt, yt, f)
      case _ => throw new Exception();
    }
  }

  def g(env:Map[Id.T, (List[(Id.T, Type.T)], T)], e:T):T = {
    e match {
      case IfEq(x, y, e1, e2) => IfEq(x, y, g(env, e1), g(env, e2))
      case IfLE(x, y, e1, e2) => IfLE(x, y, g(env, e1), g(env, e2))
      case Let(xt, e1, e2) => Let(xt, g(env, e1), g(env, e2))

      case LetRec(Fundef((x, t), args, body), e2) =>
          val envp = if (size(body) > threshold) env else env + (x->(args, body))
          LetRec(Fundef((x, t), args, g(envp, body)), g(envp, e2))

      case App(x, ys) if env.get(x).isDefined =>
        val (zs, e) = env(x)
        println( "inlining "+x+"@.")
        val envp = foldLeft2(
          Map[Id.T, Id.T](),
          zs,
          ys,
          (envp, x, y) => x match { case (z, t) => envp + (z -> y) }
        )
        Alpha.g(envp, e.asInstanceOf[Alpha.T]).asInstanceOf[Inline.T]

      case LetTuple(xts, y, ep) => LetTuple(xts, y, g(env, ep))
      case ep => ep
    }
  }

  def f(e:T):T = {
    g(Map.empty[Id.T, (List[(Id.T, Type.T)], T)], e)
  }

  def apply(e:Inline.T):ConstFold.T = f(e).asInstanceOf[ConstFold.T]
}
