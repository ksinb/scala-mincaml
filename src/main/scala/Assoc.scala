package mincaml

object Assoc extends KNormal{

  def main(args:Array[String]) = {

    val hoge = Syntax.Let(("x",Type.Int()),Syntax.Let(("y",Type.Int()),Syntax.Int(3),Syntax.Add(Syntax.Var("y"),Syntax.Var("y"))),Syntax.Sub(Syntax.Var("x"),Syntax.Var("x")))

    val kn = new KNormal
    val d = kn.f(hoge)
    println(d)

    val al = Alpha.f(d.asInstanceOf[Alpha.T])
    println(al)

    val bt = Beta.f(al.asInstanceOf[Beta.T])
    println(bt)

    println(f(bt.asInstanceOf[Assoc.T]))

  }


  def f(e:T):T = {
    e match {
      case IfEq(x, y, e1, e2) =>
        IfEq(x, y, f(e1), f(e2))

      case IfLE(x, y, e1, e2) =>
        IfLE(x, y, f(e1), f(e2))

      case Let(xt, e1, e2) =>
        def insert(ep:T):T = {
          ep match {
            case Let(yt, e3, e4) => Let(yt, e3, insert(e4))
            case LetRec(fundefs, eq) => LetRec(fundefs, insert(eq))
            case LetTuple(yts, z, eq) => LetTuple(yts, z, insert(eq))
            case eq => Let(xt, eq, f(e2))
          }
        }
        insert(f(e1))

      case LetRec(fundefs, e2) =>
        LetRec(
          fundefs.map(fd => Fundef(fd.name, fd.args, fd.body)),
          f(e2)
        )

      case LetTuple(xts, y, e) =>
        LetTuple(xts, y, f(e))

      case ep => ep
    }
  }
}
