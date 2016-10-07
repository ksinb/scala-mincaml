package mincaml

import java.util.NoSuchElementException

object RegAlloc extends RegAlloc {
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
    val sm = Simm.f(vt.asInstanceOf[Simm.Prog])
    println(sm)
    println(f(sm.asInstanceOf[RegAlloc.Prog]))
  }
  */
  def apply(e:Prog):Emit.Prog = f(e).asInstanceOf[Emit.Prog]
}

class RegAlloc extends Asm {

  def targetp(src:Id.T, t:(Id.T, Type.T), e:Exp):(Boolean, List[Id.T]) = {
    val (dest, t1) = t

    e match {
      case Mov(x) if x == src && is_reg(dest) =>
        //assert
        //assert
        (false, List(dest))

      case FMovD(x) if x == src && is_reg(dest) =>
        //assert
        (false, List(dest))

      case IfEq(_, _, e1, e2) =>
        val (c1, rs1) = target(src, (dest, t1), e1)
        val (c2, rs2) = target(src, (dest, t1), e2)
        (c1 && c2, rs1:::rs2)
      case IfLE(_, _, e1, e2) =>
        val (c1, rs1) = target(src, (dest, t1), e1)
        val (c2, rs2) = target(src, (dest, t1), e2)
        (c1 && c2, rs1:::rs2)
      case IfGE(_, _, e1, e2) =>
        val (c1, rs1) = target(src, (dest, t1), e1)
        val (c2, rs2) = target(src, (dest, t1), e2)
        (c1 && c2, rs1:::rs2)
      case IfFEq(_, _, e1, e2) =>
        val (c1, rs1) = target(src, (dest, t1), e1)
        val (c2, rs2) = target(src, (dest, t1), e2)
        (c1 && c2, rs1:::rs2)
      case IfFLE(_, _, e1, e2) =>
        val (c1, rs1) = target(src, (dest, t1), e1)
        val (c2, rs2) = target(src, (dest, t1), e2)
        (c1 && c2, rs1:::rs2)

      case CallCls(x, ys, zs) =>
        val reg_cl_list = if (x == src) List(reg_cl) else List.empty[Id.T]
        (
          true,
          target_args(src, regs, 0, ys) ::: target_args(src, fregs, 0, zs) ::: reg_cl_list
        )

      case CallDir(_, ys, zs) =>
        (
          true,
          target_args(src, regs, 0, ys) ::: target_args(src, fregs, 0, zs)
        )

      case _ => (false, List())
    }
  }

  def target(src:Id.T, dest:(Id.T, Type.T), e:T):(Boolean, List[Id.T]) = {

    e match {
      case Ans(exp) => targetp(src, dest, exp)
      case Let(xt, exp, e1) =>
        val (c1, rs1) = targetp(src, xt, exp)
        if (c1) {
          (true, rs1)
        } else {
          val (c2, rs2) = target(src, dest, e1)
          (c2, rs1 ::: rs2)
        }
    }
  }

  def target_args(src:Id.T, all:Array[Id.T], n:Int, e:List[Id.T]):List[Id.T] = {
    e match {
      case List() => List()
      case y :: ys if src == y =>
        all(n) :: target_args(src, all, n+1, ys)

      case _ :: ys =>
        target_args(src, all, n+1, ys)
    }
  }

  def source(t:Type.T, e:T):List[Id.T] = {
    e match {
      case Ans(exp) => sourcep(t, exp)
      case Let(_, _, exp) => source(t, exp)
    }
  }

  def sourcep(t:Type.T, e:Exp):List[Id.T] = {
    e match {
      case Mov(x) => List(x)
      case Neg(x) => List(x)
      case Add(x, C(_)) => List(x)
      case Sub(x, _) => List(x)
      case FMovD(x) => List(x)
      case FNegD(x) => List(x)
      case FSubD(x, _) => List(x)
      case FDivD(x, _) => List(x)

      case Add(x, V(y)) => x::List(y)
      case FAddD(x, y) => x::List(y)
      case FMulD(x, y) => x::List(y)

      case IfEq(_, _, e1, e2) => source(t, e1) ::: source(t, e2)
      case IfLE(_, _, e1, e2) => source(t, e1) ::: source(t, e2)
      case IfGE(_, _, e1, e2) => source(t, e1) ::: source(t, e2)
      case IfFEq(_, _, e1, e2) => source(t, e1) ::: source(t, e2)
      case IfFLE(_, _, e1, e2) => source(t, e1) ::: source(t, e2)

      case CallCls(_, _, _) =>
        t match {
          case Type.Unit() => List()
          case Type.Float() => List(fregs(0))
          case _ => List(regs(0))
        }
      case CallDir(_, _, _) =>
        t match {
          case Type.Unit() => List()
          case Type.Float() => List(fregs(0))
          case _ => List(regs(0))
        }

      case _ => List()
    }
  }

  sealed abstract class AllocResult()
  case class Alloc(t:Id.T) extends AllocResult
  case class Spill(t:Id.T) extends AllocResult

  def alloc(cont:T, regenv: Map[Id.T, Id.T], x:Id.T, t:Type.T, prefer:List[Id.T]):AllocResult = {
    //assert
    val all = t match {
      case Type.Unit() => List()
      case Type.Float() => allfregs
      case _ => allregs
    }

    if (all == List()) {
      Alloc("%unit")
    } else {
      if (is_reg(x)) {
        Alloc(x)
      } else {
        val free = fv(cont)

        try {
          val live = free.foldLeft(List.empty[Id.T]) {
            (live, y) => {
              if (is_reg(y)) {
                y :: live
              } else {
                try {
                  regenv(y) :: live
                } catch {
                  case _ => live
                }
              }
            }
          }

          val r = (prefer ::: all).find(
            (r) => ! live.contains(r)
          )

          Alloc(r.get)

        } catch {
          case _ =>
            println("register allocation failed")
            val y = free.reverse.find((y)=>
              ! is_reg(y)
            )
            println("spilling from")
            Spill(y.get)
        }
      }
    }
  }


  def add(x:Id.T, r:Id.T, regenv:Map[Id.T, Id.T]):Map[Id.T, Id.T] = {
    if (is_reg(x)) {
      //assert
      regenv
    } else {
      regenv+(x->r)
    }
  }

  case class NoReg(a:Id.T, b:Type.T) extends Exception
  def find(x:Id.T, t:Type.T, regenv:Map[Id.T, Id.T]):Id.T = {
    if (is_reg(x)) {
      x
    } else {
      try {
        regenv(x)
      } catch {
        case _ => throw new NoReg(x, t)
      }
    }
  }

  def findp(xp:id_or_imm, regenv:Map[Id.T, Id.T]) = {
    xp match {
      case V(x) => V(find(x, Type.Int(), regenv))
      case c => c
    }
  }

  def g(dest:(Id.T, Type.T), cont:T, regenv:Map[Id.T, Id.T], e:T):(T, Map[Id.T, Id.T]) = {
    e match {
      case Ans(exp) => g_and_restore(dest, cont, regenv, exp)
      case Let(xt@(x, t), exp, e1) =>
        //assert
        val contp = concat(e1, dest, cont)
        val (e1p, regenv1) = g_and_restore(xt, contp, regenv, exp)
        val (_, targets) = target(x, dest, contp)
        val sources = source(t, e1p)

        alloc(contp, regenv1, x, t, targets:::sources) match {
          case Spill(y) =>
            val r = regenv1(y)
            val (e2p, regenv2) = g(dest, cont, regenv1-y+(x->r), e1)
            val save = try {
              Save(regenv(y), y)
            } catch {
              case _ => Nop()
            }
            (seq(save, concat(e1p, (r, t), e2p)), regenv2)

          case Alloc(r) =>
            val (e2p, regenv2) = g(dest, cont, regenv1+(x->r), e1)
            (concat(e1p, (r, t), e2p), regenv2)
        }
    }
  }

  def g_and_restore(dest:(Id.T, Type.T), cont:T, regenv:Map[Id.T, Id.T], exp:Exp):(T, Map[Id.T, Id.T]) = {
    try {
      gp(dest, cont, regenv, exp)
    } catch {
      case NoReg(x, t) => g(dest, cont, regenv, Let((x, t), Restore(x), Ans(exp)))
    }
  }

  def gp(dest:(Id.T, Type.T), cont:T, regenv:Map[Id.T, Id.T], e:Exp):(T, Map[Id.T, Id.T]) = {
    e match{
      case Nop() => (Ans(e), regenv)
      case Set(_) => (Ans(e), regenv)
      case SetL(_) => (Ans(e), regenv)
      case Comment(_) => (Ans(e), regenv)
      case Restore(_) => (Ans(e), regenv)

      case Mov(x) => (Ans(Mov(find(x, Type.Int(), regenv))), regenv)
      case Neg(x) => (Ans(Neg(find(x, Type.Int(), regenv))), regenv)

      case Add(x, yp) => (Ans(Add(find(x, Type.Int(), regenv), findp(yp, regenv))), regenv)
      case Sub(x, yp) => (Ans(Sub(find(x, Type.Int(), regenv), findp(yp, regenv))), regenv)

      case Ld(x, y, i) =>
        (Ans(Ld(find(x, Type.Int(), regenv), findp(y, regenv), i)), regenv)
      case St(x, y, z, i) =>
        (Ans(St(find(x, Type.Int(), regenv), find(y, Type.Int(), regenv), findp(z, regenv), i)), regenv)

      case FMovD(x) => (Ans(FMovD(find(x, Type.Float(), regenv))), regenv)
      case FNegD(x) => (Ans(FNegD(find(x, Type.Float(), regenv))), regenv)

      case FAddD(x, y) => (Ans(FAddD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
      case FSubD(x, y) => (Ans(FSubD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
      case FMulD(x, y) => (Ans(FMulD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)
      case FDivD(x, y) => (Ans(FDivD(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv))), regenv)

      case LdDF(x, y, i) =>
        (Ans(LdDF(find(x, Type.Int(), regenv), findp(y, regenv), i)), regenv)
      case StDF(x, y, z, i) =>
        (Ans(StDF(find(x, Type.Int(), regenv), find(y, Type.Int(), regenv), findp(z, regenv), i)), regenv)

      case exp@IfEq(x, y, e1, e2) =>
        g_if(dest, cont, regenv, exp, (e1p, e2p) => IfEq(find(x, Type.Int(), regenv), findp(y, regenv), e1p, e2p), e1, e2)
      case exp@IfLE(x, y, e1, e2) =>
        g_if(dest, cont, regenv, exp, (e1p, e2p) => IfLE(find(x, Type.Int(), regenv), findp(y, regenv), e1p, e2p), e1, e2)
      case exp@IfGE(x, y, e1, e2) =>
        g_if(dest, cont, regenv, exp, (e1p, e2p) => IfGE(find(x, Type.Int(), regenv), findp(y, regenv), e1p, e2p), e1, e2)

      case exp@IfFEq(x, y, e1, e2) =>
        g_if(dest, cont, regenv, exp, (e1p, e2p) => IfFEq(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv), e1p, e2p), e1, e2)
      case exp@IfFLE(x, y, e1, e2) =>
        g_if(dest, cont, regenv, exp, (e1p, e2p) => IfFLE(find(x, Type.Float(), regenv), find(y, Type.Float(), regenv), e1p, e2p), e1, e2)

      case exp@CallCls(x, ys, zs) =>
        g_call(dest, cont, regenv, exp, (ysp, zsp)=>CallCls(find(x, Type.Int(), regenv), ysp, zsp), ys, zs)
      case exp@CallDir(l, ys, zs) =>
        g_call(dest, cont, regenv, exp, (ysp, zsp)=>CallDir(l, ysp, zsp), ys, zs)


      case Save(x, y) => throw new Exception()
    }
  }

  def g_if(dest:(Id.T, Type.T), cont:T, regenv:Map[Id.T, Id.T], exp:Exp, constr:(T, T)=>Exp, e1:T, e2:T) : (T, Map[Id.T, Id.T]) = {
    val (e1p, regenv1) = g(dest, cont, regenv, e1)
    val (e2p, regenv2) = g(dest, cont, regenv, e2)

    val regenvp =

      fv(cont).foldLeft(Map.empty[Id.T, Id.T]) {
        (regenvp:Map[Id.T, Id.T], x:Id.T) => {
          try {
            if (is_reg(x)) {
              regenvp
            } else {
              val r1 = regenv1(x)
              val r2 = regenv2(x)

              if (r1 != r2) {
                regenvp
              } else {
                regenvp+(x->r1)
              }
            }
          } catch {
            case _ => regenvp
          }
        }
      }

    (
      fv(cont).foldLeft(Ans(constr(e1p, e2p)).asInstanceOf[T]) {
        (e, x) => {
          if ((x == (dest match {case (a, _) => a})) || ! (regenv contains x) || (regenvp contains x)) {
            e
          } else {
            seq(Save(regenv(x), x), e)
          }
        }
      },
      regenvp
    )
  }


  def g_call(
    dest:(Id.T, Type.T),
    cont:T,
    regenv:Map[Id.T, Id.T],
    exp:Exp,
    constr:(List[Id.T], List[Id.T])=>Exp,
    ys:List[Id.T],
    zs:List[Id.T]
  ) = {

    (
      fv(cont).foldLeft(
        Ans(constr(ys.map(y=>find(y, Type.Int(), regenv)), zs.map(z=>find(z, Type.Int(), regenv)))).asInstanceOf[T]
      ) {
        (e, x) => {
          if ((x == (dest match { case (a, _) => a})) || !(regenv contains x))  {
            e
          } else {
            seq(Save(regenv(x), x), e)
          }
        }
      },
      Map.empty[Id.T, Id.T]
    )
  }

  def h(f:Fundef):Fundef = {
    val regenv1 = Map(f.name -> reg_cl)

    val (i, arg_regs, regenv2) = f.args.foldLeft((0, List.empty[Id.T], regenv1)) {
      case ((i0, arg_regs0, regenv0), y) =>
        val r = regs(i0)
        (i0 + 1, arg_regs0 ::: List(r), regenv0 + (y->r))
    }

    val (d, farg_regs, regenv3) = f.fargs.foldLeft((0, List.empty[Id.T], regenv2)) {
      case ((d0, fargs_regs0, regenv0), z) =>
        val fr = fregs(d0)
        (d0 + 1, fargs_regs0:::List(fr), regenv0 + (z->fr))
    }

    val a = f.ret match {
      case Type.Unit() => Id.gentmp(Type.Unit())
      case Type.Float() => fregs(0)
      case _ => regs(0)
    }

    val (ep, _) = g((a, f.ret), Ans(Mov(a)), regenv3, f.body)

    Fundef(f.name, arg_regs, farg_regs, ep, f.ret)
  }

  def f(prog:Prog):Prog = {
    println("register allocation: may take some time")
    val fundefsp = prog.fundefs.map(h)
    val (ep, _) = g((Id.gentmp(Type.Unit()), Type.Unit()), Ans(Nop()), Map.empty, prog.e)
    Prog(prog.data, fundefsp, ep)
  }
}
