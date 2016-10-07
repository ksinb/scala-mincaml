package mincaml

import java.io.PrintWriter

object Emit extends Emit {
  def apply(oc:out_channel, e:Prog) = f(oc, e)
}


class Emit extends Asm {
  type out_channel = PrintWriter

  var stackset = scala.collection.immutable.Set[Id.T]()
  var stackmap = List[Id.T]()

  def gethi(a:Double):Int = {
    val l = java.lang.Double.doubleToLongBits(a)
    (l>>32).asInstanceOf[Int]
  }

  def getlo(a:Double):Int = {
    java.lang.Double.doubleToLongBits(a).asInstanceOf[Int]
  }

  def save(x:Id.T) = {
    stackset += x
    if (!stackmap.contains(x)) {
      stackmap = stackmap ::: List(x)
    }
  }

  def savef(x:Id.T) = {
    stackset += x

    if (!stackmap.contains(x)) {
      val pad = if (stackmap.length % 2 == 0) {
        List()
      } else {
        List(Id.gentmp(Type.Int()))
      }
      stackmap = stackmap ::: pad ::: List(x, x)
    }
  }

  def locate(x:Id.T):List[Int] = {
    def loc(w:List[Id.T]):List[Int] = {
      w match {
        case List() => List()
        case y :: zs if x == y => 0 :: loc(zs).map{_+1}
        case y :: zs => loc(zs).map{_+1}
      }
    }
    loc(stackmap)
  }

  def offset(x:Id.T):Int = {
    4 * locate(x).head
  }

  def stacksize():Int = align(stackmap.length* 4)

  def pp_id_or_imm(e:id_or_imm):String = {
    e match {
      case V(x) => x
      case C(i) => "$" + i.toString
    }
  }

  def shuffle(sw:Id.T, xys:List[(Id.T, Id.T)]):List[(Id.T, Id.T)] = {
    val (_, xys1) = xys.partition((x) => x._1 == x._2)

    xys1.partition((x) => xys1.toMap contains x._2) match {
      case (List(), List()) => List()

      case ((x, y) :: xys2, List()) =>
        (y, sw) ::(x, y) :: shuffle(sw, xys2.map {
          case (yp, z) if yp == y => (sw, z)
          case yz => yz
        })

      case (xys2, acyc) => acyc ::: shuffle(sw, xys2)
    }
  }

  sealed abstract class Dest()
  case class Tail() extends Dest
  case class NonTail(t:Id.T) extends Dest

  def g(oc:out_channel, e:(Dest, T)):Unit = {
    e match {
      case (dest, Ans(exp)) => gp(oc, (dest, exp))
      case (dest, Let((x, t), exp, e1)) =>
        gp(oc, (NonTail(x), exp))
        g(oc, (dest, e1))
    }
  }

  def gp(oc:out_channel, e:(Dest, Exp)):Unit = {

    e match {
      case (NonTail(_), Nop()) =>

      case (NonTail(x), Set(i)) =>
        oc.println("\tmovl\t$%d, %s".format(i, x))

      case (NonTail(x), SetL(y)) =>
        oc.println("\tmovl\t$%s, %s".format(y, x))

      case (NonTail(x), Mov(y)) =>
        if (x != y) {
          oc.println("\tmovl\t%s, %s".format(y, x))
        }

      case (NonTail(x), Neg(y)) =>
        if (x != y) {
          oc.println("\tmovl\t%s, %s".format(y, x))
        }
        oc.println("\tnegl\t%s".format(x))

      case (NonTail(x), Add(y, zp)) =>
        if (x == pp_id_or_imm(zp)) {
          oc.println("\taddl\t%s, %s".format(y, x))
        } else {
          if (x != y) {
            oc.println("\tmovl\t%s, %s".format(y, x))
          }
          oc.println("\taddl\t%s, %s".format(pp_id_or_imm(zp), x))
        }

      case (NonTail(x), Sub(y, zp)) =>
        if (x == pp_id_or_imm(zp)) {
          oc.println("\tsubl\t%s, %s".format(y, x))
          oc.println("\tnegl\t%s".format(x))
        } else {
          if (x != y) {
            oc.println("\tmovl\t%s, %s".format(y, x))
          }
          oc.println("\tsubl\t%s, %s".format(pp_id_or_imm(zp), x))
        }

      case (NonTail(x), Ld(y, V(z), i)) =>
        oc.println("\tmovl\t(%s, %s, %d), %s".format(y, z, i, x))

      case (NonTail(x), Ld(y, C(j), i)) =>
        oc.println("\tmovl\t%d(%s), %s".format(j * i, y, x))

      case (NonTail(_), St(x, y, V(z), i)) =>
        oc.println("\tmovl\t%s, (%s, %s, %d)".format(x, y, z, i))

      case (NonTail(_), St(x, y, C(j), i)) =>
        oc.println("\tmovl\t%s, %d(%s)".format(x, j * i, y))

      case (NonTail(x), FMovD(y)) if x != y =>
        oc.println("\tmovsd\t%s, %s".format(y, x))

      case (NonTail(x), FNegD(y)) =>
        if (x != y) {
          oc.println("\tmovsd\t%s, %s".format(y, x))
        }
        oc.println("\txorpd\tmin_caml_fnegd, %s".format(x))

      case (NonTail(x), FAddD(y, z)) =>
        if (x == z) {
          oc.println("\taddsd\t%s, %s".format(y, x))
        } else {
          if (x != y) {
            oc.println("\tmovsd\t%s, %s".format(y, x))
          }
          oc.println("\taddsd\t%s, %s".format(z, x))
        }


      case (NonTail(x), FSubD(y, z)) =>
        if (x == z) {
          val ss = stacksize()
          oc.println("\tmovsd\t%s, %d(%s)".format(z, ss, reg_sp))
          if (x != y) {
            oc.println("\tmovsd\t%s, %s".format(y, x))
          }
          oc.println("\tsubsd\t%d(%s), %s".format(ss, reg_sp, x))
        } else {
          if (x != y) {
            oc.println("\tmovsd\t%s, %s".format(y, x))
          }
          oc.println("\tsubsd\t%s, %s".format(z, x))
        }

      case (NonTail(x), FMulD(y, z)) =>
        if (x == z) {
          oc.println("\tmulsd\t%s, %s".format(y, x))
        } else {
          if (x != y) {
            oc.println("\tmovsd\t%s, %s".format(y, x))
          }
          oc.println("\tmulsd\t%s, %s".format(z, x))
        }

      case (NonTail(x), FDivD(y, z)) =>
        if (x == z) {
          val ss = stacksize()
          oc.println("\tmovsd\t%s, %d(%s)".format(z, ss, reg_sp))
          if (x != y) {
            oc.println("\tmovsd\t%s, %s".format(y, x))
          }
          oc.println("\tdivsd\t%d(%s), %s".format(ss, reg_sp, x))
        } else {
          if (x != y) {
            oc.println("\tmovsd\t%s, %s".format(y, x))
          }
          oc.println("\tdivsd\t%s, %s".format(z, x))
        }

      case (NonTail(x), LdDF(y, V(z), i)) =>
        oc.println("\tmovsd\t(%s, %s, %d), %s".format(y, z, i, x))

      case (NonTail(x), LdDF(y, C(j), i)) =>
        oc.println("\tmovsd\t%d(%s), %s".format(j*i, y, x))

      case (NonTail(_), StDF(x, y, V(z), i)) =>
        oc.println("\tmovsd\t%s, (%s, %s, %d)\n".format(x, y, z, i))

      case (NonTail(_), StDF(x, y, C(j), i)) =>
        oc.println("\tmovsd\t%s, %d(%s)".format(x, j*i, y))

      case (NonTail(_), Save(x, y)) if allregs.contains(x) && !stackset.contains(y) =>
        save(y)
        oc.println("\tmovl\t%s, %d(%s)".format(x, offset(y), reg_sp))

      case (NonTail(_), Save(x, y)) if allfregs.contains(x) && !stackset.contains(y) =>
        savef(y)
        oc.println("\tmovsd\t%s, %d(%s)".format(x, offset(y), reg_sp))

      case (NonTail(x), Restore(y)) if allregs.contains(x) =>
        oc.println("\tmovl\t%d(%s), %s".format(offset(y), reg_sp, x))

      case (NonTail(x), Restore(y)) =>
        oc.println("\tmovsd\t%d(%s), %s".format(offset(y), reg_sp, x))

      case (Tail(), exp@(Nop()|St(_,_,_,_)|StDF(_,_,_,_)|Save(_,_)) ) =>
        gp(oc, (NonTail(Id.gentmp(Type.Unit())), exp))
        oc.println("\tret")

      case (Tail(), exp@(Set(_)|SetL(_)|Mov(_)|Neg(_)|Add(_,_)|Sub(_,_)|Ld(_,_,_))) =>
        gp(oc, (NonTail(regs(0)), exp))
        oc.println("\tret")

      case (Tail(), exp@(FMovD(_)|FNegD(_)|FAddD(_,_)|FSubD(_,_)|FMulD(_,_)|FDivD(_,_)|LdDF(_,_,_))) =>
        gp(oc, (NonTail(fregs(0)), exp))
        oc.println("\tret")

      case (Tail(), exp@Restore(x)) =>
        locate(x) match {
          case List(i) => gp(oc, (NonTail(regs(0)), exp))
          case i :: List(j) if i + 1 == j => gp(oc, (NonTail(fregs(0)), exp))
        }

      case (Tail(), IfEq(x, yp, e1, e2)) =>
        oc.println("\tcmpl\t%s, %s".format(pp_id_or_imm(yp), x))
        g_tail_if(oc, e1, e2, "je", "jne")

      case (Tail(), IfLE(x, yp, e1, e2)) =>
        oc.println("\tcmpl\t%s, %s".format(pp_id_or_imm(yp), x))
        g_tail_if(oc, e1, e2, "jle", "jg")

      case (Tail(), IfGE(x, yp, e1, e2)) =>
        oc.println("\tcmpl\t%s, %s".format(pp_id_or_imm(yp), x))
        g_tail_if(oc, e1, e2, "jge", "jl")

      case (Tail(), IfFEq(x, y, e1, e2)) =>
        oc.println("\tcomisd\t%s, %s".format(y, x))
        g_tail_if(oc, e1, e2, "je", "jne")

      case (Tail(), IfFLE(x, y, e1, e2)) =>
        oc.println("\tcomisd\t%s, %s".format(y, x))
        g_tail_if(oc, e1, e2, "jbe", "ja")

      case (NonTail(z), IfEq(x, yp, e1, e2)) =>
        oc.println("\tcmpl\t%s, %s".format(pp_id_or_imm(yp), x))
        g_non_tail_if(oc, NonTail(z), e1, e2, "je", "jne")

      case (NonTail(z), IfLE(x, yp, e1, e2)) =>
        oc.println("\tcmpl\t%s, %s".format(pp_id_or_imm(yp), x))
        g_non_tail_if(oc, NonTail(z), e1, e2, "jle", "jg")

      case (NonTail(z), IfGE(x, yp, e1, e2)) =>
        oc.println("\tcmpl\t%s, %s".format(pp_id_or_imm(yp), x))
        g_non_tail_if(oc, NonTail(z), e1, e2, "jge", "jl")

      case (NonTail(z), IfFEq(x, y, e1, e2)) =>
        oc.println("\tcmpl\t%s, %s".format(y, x))
        g_non_tail_if(oc, NonTail(z), e1, e2, "je", "jne")

      case (NonTail(z), IfFLE(x, y, e1, e2)) =>
        oc.println("\tcmpl\t%s, %s".format(y, x))
        g_non_tail_if(oc, NonTail(z), e1, e2, "jbe", "ja")

      case (Tail(), CallCls(x, ys, zs)) =>
        g_args(oc, List((x, reg_cl)), ys, zs)
        oc.println("\tjmp\t*(%s)".format(reg_cl))

      case (Tail(), CallDir(x, ys, zs)) =>
        g_args(oc, List(), ys, zs)
        oc.println("\tjmp\t%s".format(x))

      case (NonTail(a), CallCls(x, ys, zs)) =>
        g_args(oc, List((x, reg_cl)), ys, zs)
        val ss = stacksize()
        if (ss > 0) {
          oc.println("\taddl\t$%d, %s".format(ss, reg_sp))
        }

        oc.println("\tcall\t*(%s)".format(reg_cl))
        if (ss > 0) {
          oc.println("\tsubl\t$%d, %s".format(ss, reg_sp))
        }

        if (allregs.contains(a) && a != regs(0)) {
          oc.println("\tmovl\t%s, %s".format(regs(0), a))
        } else if (allfregs.contains(a) && a != fregs(0)) {
          oc.println("\tmovsd\t%s, %s".format(fregs(0), a))
        }

      case (NonTail(a), CallDir(x, ys, zs)) =>
        g_args(oc, List(), ys, zs)
        val ss = stacksize()
        if (ss > 0) {
          oc.println("\taddl\t$%d, %s".format(ss, reg_sp))
        }
        oc.println("\tcall\t%s".format(x))

        if (ss > 0) {
          oc.println("\tsubl\t$%d, %s".format(ss, reg_sp))
        }

        if (allregs.contains(a) && a != regs(0)) {
          oc.println("\tmovl\t%s, %s".format(regs(0), a))
        } else if (allfregs.contains(a) && a != fregs(0)) {
          oc.println("\tmovsd\t%s, %s".format(fregs(0), a))
        }
    }
  }

  def g_tail_if(oc:out_channel, e1:T, e2:T, b:String, bn:String):Unit = {
    val b_else = Id.genid(b + "_else")
    oc.println("\t%s\t%s".format(bn, b_else))

    val stackset_back = stackset
    g(oc, (Tail(), e1))
    oc.println("%s:".format(b_else))

    stackset = stackset_back
    g(oc, (Tail(), e2))
  }

  def g_non_tail_if(oc:out_channel, dest:Dest, e1:T, e2:T, b:String, bn:String):Unit = {
    val b_else = Id.genid(b + "_else")
    val b_cont = Id.genid(b + "_cont")
    oc.println("\t%s\t%s".format(bn, b_else))

    val stackset_back = stackset
    g(oc, (dest, e1))

    val stackset1 = stackset
    oc.println("\tjmp\t%s".format(b_cont))
    oc.println("%s:".format(b_else))

    stackset = stackset_back
    g(oc, (dest, e2))
    oc.println("%s:".format(b_cont))

    val stackset2 = stackset
    stackset = stackset1 intersect stackset2
  }

  def g_args(oc:out_channel, x_reg_cl:List[(Id.T, Id.T)], ys:List[Id.T], zs:List[Id.T]) = {
    //assert
    //assert
    val sw = "%d(%s)".format(stacksize(), reg_sp)

    val (i, yrs) = ys.foldLeft((0, x_reg_cl)) {
      case ((j, zrs), y) => (j+1, (y, regs(j)) :: zrs)
    }
    shuffle(sw, yrs).foreach{
      case (y, r) => oc.println("\tmovl\t%s, %s".format(y, r))
    }
    val (d, zfrs) = zs.foldLeft((0, List[(Id.T, String)]())){
      case ((dp, zfs), z) => (dp+1, (z, fregs(dp)) :: zfs)
    }
    shuffle(sw, zfrs).foreach{
      case (z, fr) => oc.println("\tmovsdxxx\t%s, %s".format(z, fr))
    }
  }

  def h(oc:out_channel, f:Fundef) = {
    f match {
      case Fundef(x, _, _, e, _) =>
        oc.println("%s:".format(x))
        stackset = scala.collection.immutable.Set[Id.T]()
        stackmap = List[Id.T]()
        g(oc, (Tail(), e))
    }
  }

  def f(oc:out_channel, prog:Prog) = {

    prog match {
      case Prog(data, fundefs, e) =>
        oc.println(".data")
        oc.println(".balign\t8")
        data.foreach{
          case (x, d) =>
            oc.println("%s:\t# %f".format(x, d))
            oc.println("\t.long\t0x%lx".format(gethi(d)))
            oc.println("\t.long\t0x%lx".format(getlo(d)))
        }
        oc.println(".text")
        fundefs.foreach{
          fundef => h(oc, fundef)
        }
        oc.println(".global\tmin_caml_start")
        oc.println("mincaml_start:")
        oc.println(".globl\t_min_caml_start")
        oc.println("_min_caml_start: # for cygwin")
        oc.println("\tpushl\t%eax")
        oc.println("\tpushl\t%ebx")
        oc.println("\tpushl\t%ecx")
        oc.println("\tpushl\t%edx")
        oc.println("\tpushl\t%esi")
        oc.println("\tpushl\t%edi")
        oc.println("\tpushl\t%ebp")
        oc.println("\tmovl\t32(%%esp), %s".format(reg_sp))
        oc.println("\tmovl\t36(%%esp), %s".format(regs(0)))
        oc.println("\tmovl\t%s,%s".format(regs(0), reg_hp))
        stackset = scala.collection.immutable.Set[Id.T]()
        stackmap = List[Id.T]()
        g(oc, (NonTail(regs(0)), e))
        oc.println("\tpopl\t%ebp")
        oc.println("\tpopl\t%edi")
        oc.println("\tpopl\t%esi")
        oc.println("\tpopl\t%edx")
        oc.println("\tpopl\t%ecx")
        oc.println("\tpopl\t%ebx")
        oc.println("\tpopl\t%eax")
        oc.println("\tret")
    }
    oc.close()
  }
}
