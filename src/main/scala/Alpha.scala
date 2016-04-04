package mincaml

class Alpha extends kNormal{

  def g(env:Map[Id.T, Id.T], e:T):T = {
    e match {
      case Unit() => Unit()
      case Int(i) => Int(i)
      case Float(d) => Float(d)
      case Neg(x) => Neg(env(x))
      case Add(x, y) => Add(env(x), env(y))
      case Sub(x, y) => Sub(env(x), env(y))
      case FNeg(x) => FNeg(env(x))
      case FAdd(x, y) => FAdd(env(x), env(y))
      case FSub(x, y) => FSub(env(x), env(y))
      case FMul(x, y) => FMul(env(x), env(y))
      case FDiv(x, y) => FDiv(env(x), env(y))

      case IfEq(x, y, e1, e2) => IfEq(env(x), env(y), g(env, e1), g(env, e2))
      case IfLE(x, y, e1, e2) => IfLE(env(x), env(y), g(env, e1), g(env, e2))
      case Let((x, t), e1, e2) =>
        val xp = Id.genid(x)
        Let((xp, t), g(env, e1), g(env+(x->xp), e2))

      case Var(x) => Var(env(x))

      case LetRec(Fundef((x,t), yts, e1), e2) =>
        val env1 = env + (x->Id.genid(x))
        val ys = yts.map((y)=>y._1)
        //val envp = ys ++ (ys.map((y)=>Id.genid(y)), env1).zipped.map((_,_)) ////need modify
        val envp = yts.foldLeft(env1){case (ep, (k, _))=>ep+(k->Id.genid(k))}

        LetRec(
          Fundef(
            (env(x), t),
            yts.map{ case (y, t1) => (envp(y), t1) },
            g(envp, e1)
          ),
          g(env, e2)
        )


      case App(x, ys) => App(env(x), ys.map((y)=>env(y)))
      case Tuple(xs) => Tuple(xs.map((x)=>env(x)))

      case LetTuple(xts, y, e2) =>
        val xs = xts.map((x)=>x._1)
        val envp = xts.foldLeft(env){ case (e1, (k, _)) => e1+(k->Id.genid(k)) }   //need modify

        LetTuple(
          xts.map{ case (x, t) => (envp(x), t) },
          env(y),
          g(envp, e2)
        )


      case Get(x, y) => Get(env(x), env(y))
      case Put(x, y, z) => Put(env(x), env(y), env(z))
      case ExtArray(x) => ExtArray(x)
      case ExtFunApp(x, ys) => ExtFunApp(x, ys.map((y)=>env(y)))

    }
    
  }

  def f(e:T) = {
    g(Map.empty[Id.T, Id.T], e)
  }
}
