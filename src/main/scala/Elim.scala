package mincaml

object Elim extends KNormal{

  def main(args:Array[String]) = {

    val parser = new Parser
    val pr = parser.parse("let x = 3 in let y = 7 in 10")

    val typing = new Typing
    val tp = typing.f(pr.asInstanceOf[typing.T])

    val knormal = new KNormal
    val kn = knormal.f(tp.asInstanceOf[Syntax.T])
    val al = Alpha.f(kn.asInstanceOf[Alpha.T])
    val bt = Beta.f(al.asInstanceOf[Beta.T])
    val as = Assoc.f(bt.asInstanceOf[Assoc.T])
    val il = Inline.f(as.asInstanceOf[Inline.T])
    val cf = ConstFold.f(il.asInstanceOf[ConstFold.T])
    println("cf", cf)
    println("em", f(cf.asInstanceOf[Elim.T]))

  }

    def effect(e:T):scala.Boolean = {
      e match {
        case Let(_, e1, e2) => effect(e1) || effect(e2)
        case IfEq(_, _, e1, e2) => effect(e1) || effect(e2)
        case IfLE(_, _, e1, e2) => effect(e1) || effect(e2)

        case LetRec(_, e1) => effect(e1)
        case LetTuple(_, _, e1) => effect(e1)

        case App(_, _) => true
        case Put(_, _, _) => true
        case ExtFunApp(_, _) => true
        case _ => false
      }
    }

    def f(e:T):T = {
      e match {
        case IfEq(x, y, e1, e2) => IfEq(x, y, f(e1), f(e2))
        case IfLE(x, y, e1, e2) => IfLE(x, y, f(e1), f(e2))

        case Let((x, t), e1, e2) =>
          val e1p = f(e1)
          val e2p = f(e2)
          if (effect(e1p) || fv(e2p)(x)) Let((x, t), e1p, e2p)
          else println("eliminating variable " + x + "@."); e2p

        case LetRec(Fundef((x, t), args, body), e2) =>
          val e2p = f(e2)
          if (fv(e2p) contains x) LetRec(Fundef((x, t), args, body), e2)
          else println("eliminating function " + x + "@."); e2p

        case LetTuple(xts, y, e2) =>
          val xs = xts.map(xt=>xt._1)
          val ep = f(e)
          val live = fv(ep)
          if (xs.exists(x=>live(x))) LetTuple(xts, y, ep)
          else println("eliminating variables " + Id.pp_list(xs) + "@."); ep

        case ep => ep
      }
    }
}
