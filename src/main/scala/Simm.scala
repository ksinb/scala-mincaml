package mincaml

object Simm extends Simm{
/*
  def main(args: Array[String]) = {
    val parser = new Parser
    val pr = parser.parse("let rec fib n = if n <= 1 then n else fib(n - 1) + fib(n -2) in fib 10")
    //val pr = parser.parse("let rec quad x = let rec double x = x + x in double (double x) in quad 123")
    //val pr = parser.parse("let rec make_adder x = let rec adder y = x + y in adder in (make_adder 3) 7")

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
    val cl = Closure.f(el.asInstanceOf[KNormal.T])
    val vt = Virtual.f(cl)

    println(f(vt.asInstanceOf[Simm.Prog]))
  }
  */
  def apply(e:Simm.Prog):RegAlloc.Prog = {
    f(e).asInstanceOf[RegAlloc.Prog]
  }
}

class Simm extends Asm {

  def g(env:Map[Id.T,Int], t:T): T = {
    t match {
      case Ans(exp) => Ans(gp(env, exp))
      case Let((x, t1), Set(i), e) =>
        val ep = g(env+(x->i), e)

        if (fv(ep) contains x)
          Let((x, t1), Set(i), ep)
        else
          ep
      case Let(xt, exp, e) =>
        Let(xt, gp(env, exp), g(env, e))
    }
  }

  def gp(env:Map[Id.T,Int], e:Exp):Exp = {
    e match {
      case Add(x, V(y)) if env contains y => Add(x, C(env(y)))
      case Add(x, V(y)) if env contains x => Add(y, C(env(x)))
      case Sub(x, V(y)) if env contains y => Sub(x, C(env(y)))

      case Ld(x, V(y), i) if env contains y => Ld(x, C(env(y)), i)
      case St(x, y, V(z), i) if env contains z => St(x, y, C(env(z)), i)
      case LdDF(x, V(y), i) if env contains y => LdDF(x, C(env(y)), i)
      case StDF(x, y, V(z), i) if env contains z => StDF(x, y, C(env(z)), i)

      case IfEq(x, V(y), e1, e2) if env contains y => IfEq(x, C(env(y)), g(env, e1), g(env, e2))
      case IfLE(x, V(y), e1, e2) if env contains y => IfLE(x, C(env(y)), g(env, e1), g(env, e2))
      case IfGE(x, V(y), e1, e2) if env contains y => IfGE(x, C(env(y)), g(env, e1), g(env, e2))

      case IfEq(x, V(y), e1, e2) if env contains x => IfEq(y, C(env(x)), g(env, e1), g(env, e2))
      case IfLE(x, V(y), e1, e2) if env contains y => IfLE(y, C(env(x)), g(env, e1), g(env, e2))
      case IfGE(x, V(y), e1, e2) if env contains y => IfGE(y, C(env(x)), g(env, e1), g(env, e2))

      case IfEq(x, y, e1, e2) => IfEq(x, y, g(env, e1), g(env, e2))
      case IfLE(x, y, e1, e2) => IfLE(x, y, g(env, e1), g(env, e2))
      case IfGE(x, y, e1, e2) => IfGE(x, y, g(env, e1), g(env, e2))
      case IfFEq(x, y, e1, e2) => IfFEq(x, y, g(env, e1), g(env, e2))
      case IfFLE(x, y, e1, e2) => IfFLE(x, y, g(env, e1), g(env, e2))

      case ep => ep
    }
  }

  def h(fd:Fundef):Fundef = {
    Fundef(fd.name, fd.args, fd.fargs, g(Map.empty[Id.T, Int], fd.body), fd.ret)
  }

  def f(prog:Prog):Prog = {
    Prog(prog.data, prog.fundefs.map(h), g(Map.empty[Id.T, Int], prog.e))
  }
}
