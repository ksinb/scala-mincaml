package mincaml

object Elim extends KNormal{

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
    val cf = ConstFold.f(il.asInstanceOf[ConstFold.T])
    println(cf)

  }


  /*
    def effect(e:T):scala.Boolean = {
      true
    }

    def f(e:T):T = {
      e match {
        case IfEq(x, y, e1, e2) => IfEq(x, y, f(e1), f(e2))
        case IfLE(x, y, e1, e2) => IfLE(x, y, f(e1), f(e2))

        case Let((x, t), e1, e2) =>
          val e1p = f(e1)
          val e2p = f(e2)
          if effect(e1p) || fv(e2p)(x) Let((x, t), e1p, e2p)
          else throw new Exception()

        case LetRec(fundefs, e2) =>
          val e2p = f(e2)
          if fv(e2p)(fundefs)


          val live = fv
      }
    }*/
}
