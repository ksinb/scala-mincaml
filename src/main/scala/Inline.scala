package mincaml

object Inline extends KNormal{

  def main(args:Array[String]) = {

    val parser = new Parser
    val pr = parser.parse("let rec fact n = if n = 0 then 1 else n + fact (n-1) in fact 3")

    val typing = new Typing
    val tp = typing.f(pr.asInstanceOf[typing.T])

    val knormal = new KNormal
    val kn= knormal.f(tp.asInstanceOf[Syntax.T])

    val al = Alpha.f(kn.asInstanceOf[Alpha.T])

    val bt = Beta.f(al.asInstanceOf[Beta.T])

    val as = Assoc.f(bt.asInstanceOf[Assoc.T])

    println(as)
    println(f(as.asInstanceOf[T]))

    /*
    LetRec(
      List(
        Fundef(
          (fact,Fun(List(Int()),Unit())),
          List((n,Int())),
          Let(
            (Ti2.8,Int()),
            Int(0),
            IfEq(
              n,
              Ti2.8,
              Int(1),
              Let(
                (Tu5.9,Unit()),
                Let(
                  (Ti4.10,Int()),
                  Let(
                    (Ti3.11,Int()),
                    Int(1),
                    Sub(n,Ti3.11)
                  ),
                  App(
                    fact.6,
                    List(Ti4.10)
                  )
                ),
                Add(n,Tu5.9)
              )))
        )
      ),
      Let(
        (Ti1.12,Int()),
        Int(3),
        App(fact.6,List(Ti1.12))
      )
    )

     LetRec(
      List(
        Fundef(
          (fact.6,Fun(List(Int()),Unit())),
          List((n,Int())),
          Let(
            (Ti2.8,Int()),
            Int(0),
            IfEq(
              n,
              Ti2.8,
              Int(1),
              Let(
                (Tu5.9,Unit()),
                Let(
                  (Ti4.10,Int()),
                  Let(
                    (Ti3.11,Int()),
                    Int(1),
                    Sub(n,Ti3.11)
                  ),
                  App(
                    fact.6,
                    List(Ti4.10)
                  )
                ),
                Add(n,Tu5.9)
              )))
              )
            ),
            Let(
              (Ti1.12,Int()),
              Int(3),
              Let((Ti2.8.13,Int()),Int(0),IfEq(Ti1.12,Ti2.8.13,Int(1),Let((Tu5.9.14,Unit()),Let((Ti4.10.15,Int()),Let((Ti3.11.16,Int()),Int(1),Sub(Ti1.12,Ti3.11.16)),App(fact.6,List(Ti4.10.15))),Add(Ti1.12,Tu5.9.14)))))
    )

    */


  }

  var threshold = 100

  def size(e:T):scala.Int = {
    e match {
      case IfEq(_, _, e1, e2) =>  1 + size(e1) + size(e2)
      case Let(_, e1, e2) => 1 + size(e1) + size(e2)
      //case LetRec(fundefs, e2) => fundefs.foldLeft(size(e2)){case (s, fd) => s + 1 + size(fd.body)}
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
        //val envp = fundefs.foldLeft(env){
//          case (ev, fd) =>
//            if (size(fd.body) > threshold) ev else ev + (fd.name._1->(fd.args, fd.body))
//        }
  //      val fundefsp = fundefs.map(fd=>Fundef(fd.name, fd.args, g(env, fd.body)))
//        LetRec(fundefsp, g(envp, e2))
          val envp = if (size(body) > threshold) env else env + (x->(args, body))
          LetRec(Fundef((x, t), args, g(env, body)), g(envp, e2))

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
}
