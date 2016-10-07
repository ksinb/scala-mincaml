package mincaml

object Assoc extends KNormal{

  def apply(e:KNormal.T):Inline.T = f(e.asInstanceOf[Assoc.T]).asInstanceOf[Inline.T]

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

      case LetRec(Fundef(name, args, body), e2) =>
        LetRec(
          Fundef(name, args, f(body)),
          f(e2)
        )

      case LetTuple(xts, y, ep) =>
        LetTuple(xts, y, f(ep))

      case ep => ep
    }
  }
}
