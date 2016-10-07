package mincaml

object Elim extends KNormal{

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
        val fve2p = fv(e2p)

        if (effect(e1p) || (fve2p contains x)) {
          Let((x, t), e1p, e2p)
        } else {
          e2p
        }

      case LetRec(Fundef((x, t), args, body), e2) =>
        val e2p = f(e2)
        val fve2p = fv(e2p)

        if (fve2p contains x) {
          LetRec(Fundef((x, t), args, f(body)), e2)
        } else {
          println("eliminating function " + x + "@."+e2p)
          e2p
        }

      case LetTuple(xts, y, e2) =>
        val xs = xts.map(xt=>xt._1)
        val ep = f(e)
        val live = fv(ep)

        if (xs.exists(x=>live(x))) {
          LetTuple(xts, y, ep)
        } else {
          println("eliminating variables " + Id.pp_list(xs) + "@.")
          ep
        }

      case ep => ep
    }
  }

  def apply(e:T):KNormal.T = {
    f(e).asInstanceOf[KNormal.T]
  }
}
