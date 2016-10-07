package mincaml

object Elim extends KNormal{
/*
  def main(args:Array[String]) = {

    val parser = new Parser
    val pr = parser.parse("let x = 3 in let y = 7 in 10")
    //val pr = parser.parse("let rec quad x = let rec double x = x + x in double (double x) in quad 123")

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
/*
    LetRec(
      Fundef(
        (quad,Fun(List(Int()),Int())),
        List((x.4,Int())),
        LetRec(
          Fundef(
            (double, Fun(List(Int()),Int())),
            List((x.6,Int())),
            Add(x.6,x.6)),
          Let(
            (Ti2.7,Int()),
            Add(x.4,x.4),
            Add(Ti2.7,Ti2.7)))),
      Let(
        (Ti1.8,Int()),
        Int(123),
        LetRec(
          Fundef(
            (double, Fun(List(Int()),Int())),
            List((x.6.10,Int())),
            Add(x.6.10,x.6.10)),
          Let(
            (Ti2.7.11,Int()),
            App(double,List(Ti1.8)),
            App(double,List(Ti2.7.11))))))

    LetRec(
      Fundef(
        (dbl,Fun(List(Int()),Int())),
        List((x.6.10,Int())),
        Add(x.6.10,x.6.10)),
      Let(
        (Ti2.7.11,Int()),
        App(dbl,List(Ti1.8)),
        App(dbl,List(Ti2.7.11)))
    )

    Let(
      (Ti1.8,Int()),
      Int(123),
      LetRec(
        Fundef(
          (double,Fun(List(Int()),Int())),
          List((x.6.10,Int())),
          Add(x.6.10,x.6.10)),
        Let(
          (Ti2.7.11,Int()),
          App(double,List(Ti1.8)),
          App(double,List(Ti2.7.11)))))
*/
  }
*/
    def apply(e:T):KNormal.T = {
      f(e).asInstanceOf[KNormal.T]
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
}
