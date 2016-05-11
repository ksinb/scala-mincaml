package mincaml

object Closure {

  case class Closure(entry:Id.L, actual_fv:List[Id.T])

  sealed abstract class T()
  case class Unit() extends T
  case class Int(i:scala.Int) extends T
  case class Float(d:scala.Double) extends T
  case class Neg(e:Id.T) extends T
  case class Add(a:Id.T, b:Id.T) extends T
  case class Sub(a:Id.T, b:Id.T) extends T
  case class FNeg(e:Id.T) extends T
  case class FAdd(a:Id.T, b:Id.T) extends T
  case class FSub(a:Id.T, b:Id.T) extends T
  case class FMul(a:Id.T, b:Id.T) extends T
  case class FDiv(a:Id.T, b:Id.T) extends T
  case class IfEq(x:Id.T, y:Id.T, e1:T, e2:T) extends T
  case class IfLE(x:Id.T, y:Id.T, e1:T, e2:T) extends T
  case class Let(name:(Id.T, Type.T), e1:T, e2:T) extends T
  case class Var(t:Id.T) extends T

  case class MakeCls(name:(Id.T, Type.T), c:Closure, t:T) extends T
  case class AppCls(a:Id.T, b:List[Id.T]) extends T
  case class AppDir(a:Id.T, b:List[Id.T]) extends T

  case class Tuple(l:List[Id.T]) extends T
  case class LetTuple(a:List[(Id.T, Type.T)], b:Id.T, c:T) extends T
  case class Get(a:Id.T, b:Id.T) extends T
  case class Put(a:Id.T, b:Id.T, c:Id.T) extends T
  case class ExtArray(t:Id.T) extends T
  case class Fundef(name:(Id.T, Type.T), args:List[(Id.T, Type.T)], formal_fv:List[(Id.T, Type.T)], body:T) extends T
  case class Prog(l:List[Fundef], t:T) extends T

  def main(args:Array[String]) = {
    val parser = new Parser
    //val pr = parser.parse("let rec quad x = let rec double x = x + x in double (double x) in quad 123")
    val pr = parser.parse("let rec make_adder x = let rec adder y = x + y in adder in (make_adder 3) 7")

    val typing = new Typing
    val tp = typing.f(pr.asInstanceOf[typing.T])

    val knormal = new KNormal
    val kn = knormal.f(tp.asInstanceOf[Syntax.T])
    val al = Alpha.f(kn.asInstanceOf[Alpha.T])
    val bt = Beta.f(al.asInstanceOf[Beta.T])
    val as = Assoc.f(bt.asInstanceOf[Assoc.T])
    val il = Inline.f(as.asInstanceOf[Inline.T])
    val cf = ConstFold.f(il.asInstanceOf[ConstFold.T])
    val el = Elim.f(cf.asInstanceOf[Elim.T])
    val cl = f(el.asInstanceOf[KNormal.T])
  }

  def fv(e:T):Set[Id.T] = {
    e match {
      case Unit() | Int(_) | Float(_) | ExtArray(_) => Set()
      case Neg(x) => Set(x)
      case FNeg(x) => Set(x)

      case Add(x, y) => Set(x, y)
      case Sub(x, y) => Set(x, y)
      case FAdd(x, y) => Set(x, y)
      case FSub(x, y) => Set(x, y)
      case FMul(x, y) => Set(x, y)
      case FDiv(x, y) => Set(x, y)
      case Get(x, y) => Set(x, y)

      case IfEq(x, y, e1, e2) => (fv(e1) union fv(e2)) + x + y
      case IfLE(x, y, e1, e2) => (fv(e1) union fv(e2)) + x + y
      case Let((x, t), e1, e2) => fv(e1) union (fv(e2) diff Set(x))
      case Var(x) => Set(x)

      case MakeCls((x, t), Closure(l, ys), e1) => ((Set() ++ ys) union fv(e1)) - x
      case AppCls(x, ys) => Set(x) ++ ys
      case AppDir(_, xs) => Set() ++ xs
      case Tuple(xs) => Set() ++ xs

      case LetTuple(xts, y, e1) => (Set() ++ xts.map(x=>x._1)) diff fv(e1) + y
      case Put(x, y, z) => Set(x, y, z)

    }
  }

  var toplevel:List[Fundef] = List()


  def g(env:Map[Id.T, Type.T], known:Set[Id.T], e:KNormal.T):T = {

    e match {
      case KNormal.Unit() => Unit()
      case KNormal.Int(i) => Int(i)
      case KNormal.Float(d) => Float(d)
      case KNormal.Neg(x) => Neg(x)
      case KNormal.Add(x, y) => Add(x, y)
      case KNormal.Sub(x, y) => Sub(x, y)
      case KNormal.FNeg(x) => FNeg(x)
      case KNormal.FAdd(x, y) => FAdd(x, y)
      case KNormal.FSub(x, y) => FSub(x, y)
      case KNormal.FMul(x, y) => FMul(x, y)
      case KNormal.FDiv(x, y) => FDiv(x, y)
      case KNormal.IfEq(x, y, e1, e2) => IfEq(x, y, g(env, known, e1), g(env, known, e2))
      case KNormal.IfLE(x, y, e1, e2) => IfLE(x, y, g(env, known, e1), g(env, known, e2))
      case KNormal.Let((x, t), e1, e2) => Let((x, t), g(env, known, e1), g(env+(x->t), known, e2))
      case KNormal.Var(x) => Var(x)

      case KNormal.LetRec(KNormal.Fundef((x, t), yts, e1), e2) =>

        val toplevel_backup = toplevel
        val envp = env + (x -> t)
        val knownp = known + x
        val e1p = g(envp ++ yts, knownp, e1)
        val zs = fv(e1p) diff (Set() ++ yts.map(y=>y._1))

        val (knownq, e1q) =
          if (zs.isEmpty) {
            (knownp, e1p)
          } else {
            println("free variable(s) "+Id.pp_list(zs.toList)+" found in function "+x+"@.")
            println("function "+x+" cannot be directly applied in fact@.")
            toplevel = toplevel_backup
            val e1r = g(envp ++ yts, known, e1)
            (known, e1r)
          }

        val zsp = (Set() ++ yts.map(y=>y._1) + x) diff fv(e1q)
        val zts = zsp.map(z=>(z, envp(z))).toList
        toplevel = Fundef((x, t), yts, zts, e1q) :: toplevel
        val e2p = g(envp, knownq, e2)

        if (fv(e2p) contains x) {
          MakeCls((x, t), Closure(x, zsp.toList), e2p)
        } else {
          println("eliminating closure(s) "+x+"@.")
          e2p
        }

      case KNormal.App(x, ys) if known(x) =>
        println("directly applying " + x + "@.")
        AppDir(x, ys)

      case KNormal.App(f, xs) => AppCls(f, xs)
      case KNormal.Tuple(xs) => Tuple(xs)
      case KNormal.LetTuple(xts, y, e1) => LetTuple(xts, y, g(env++xts, known, e1))
      case KNormal.Get(x, y) => Get(x, y)
      case KNormal.Put(x, y, z) => Put(x, y, z)
      case KNormal.ExtArray(x) => ExtArray(x)
      case KNormal.ExtFunApp(x, ys) => AppDir("mincaml_"+x, ys)
    }
  }

  def f(e:KNormal.T):Prog = {
    toplevel = List()
    val ep = g(Map(), Set(), e)
    Prog(toplevel.reverse, ep)
  }
}
