package mincaml

class Asm {
  sealed abstract class id_or_imm()
  case class V(a:Id.T) extends id_or_imm
  case class C(a:Int) extends id_or_imm

  sealed abstract class T()
  case class Ans(e:Exp) extends T
  case class Let(name:(Id.T, Type.T), e:Exp, t:T) extends T

  sealed abstract class Exp()
  case class Nop() extends Exp
  case class Set(i:Int) extends Exp
  case class SetL(i:Id.L) extends Exp

  case class Mov(i:Id.T) extends Exp
  case class Neg(i:Id.T) extends Exp
  case class Add(a:Id.T, b:id_or_imm) extends Exp
  case class Sub(a:Id.T, b:id_or_imm) extends Exp
  case class Ld(a:Id.T, b:id_or_imm, c:Int) extends Exp
  case class St(a:Id.T, b:Id.T, c:id_or_imm, d:Int) extends Exp

  case class FMovD(i:Id.T) extends Exp
  case class FNegD(i:Id.T) extends Exp
  case class FAddD(a:Id.T, b:Id.T) extends Exp
  case class FSubD(a:Id.T, b:Id.T) extends Exp
  case class FMulD(a:Id.T, b:Id.T) extends Exp
  case class FDivD(a:Id.T, b:Id.T) extends Exp

  case class LdDF(a:Id.T, b:id_or_imm, c:Int) extends Exp
  case class StDF(a:Id.T, b:Id.T, c:id_or_imm, d:Int) extends Exp
  case class Comment(s:String) extends  Exp

  case class IfEq(a:Id.T, b:id_or_imm, c:T, d:T) extends Exp
  case class IfLE(a:Id.T, b:id_or_imm, c:T, d:T) extends Exp
  case class IfGE(a:Id.T, b:id_or_imm, c:T, d:T) extends Exp
  case class IfFEq(a:Id.T, b:Id.T, c:T, d:T) extends Exp
  case class IfFLE(a:Id.T, b:Id.T, c:T, d:T) extends Exp

  case class CallCls(a:Id.T, b:List[Id.T], c:List[Id.T]) extends Exp
  case class CallDir(a:Id.T, b:List[Id.T], c:List[Id.T]) extends Exp
  case class Save(a:Id.T, b:Id.T) extends Exp
  case class Restore(a:Id.T) extends Exp

  case class Fundef(name:Id.L, args:List[Id.L], fargs:List[Id.T], body:T, ret:Type.T) extends Exp
  case class Prog(data:List[(Id.L, Double)], fundefs:List[Fundef], e:T) extends Exp

  def fletd(x:Id.T, e1:Exp, e2:T):T = Let((x, Type.Float()), e1, e2)

  def seq(e1:Exp, e2:T):T = Let((Id.gentmp(Type.Unit()), Type.Unit()), e1, e2)

  val regs = Array("%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi")
  val fregs = Array("%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7")
  val allregs = regs.toList
  val allfregs = fregs.toList
  val reg_cl = regs(regs.length -1)
  val reg_sqp = "%ebp"
  val reg_hp = "min_caml_hp"
  def is_reg(x:String):Boolean = x(0) == '%' || x == reg_hp

  def remove_and_uniq(xs:List[Id.T], e:List[Id.T]):List[Id.T] =
    e match {
      case List() => List()
      case x::ys if xs contains x => remove_and_uniq(xs, ys)
      case x::ys => x :: remove_and_uniq(x::xs, ys)
    }

  def fv_id_or_imm(a:id_or_imm):List[Id.T] =
    a match {
    case V(x) => List(x)
    case _ => List()
  }

  def fv_exp(e:Exp):List[Id.T] =
    e match {
      case Nop() => List()
      case Set(_) => List()
      case SetL(_) => List()
      case Comment(_) => List()
      case Restore(_) => List()

      case Mov(x) => List(x)
      case Neg(x) => List(x)
      case FMovD(x) => List(x)
      case FNegD(x) => List(x)
      case Save(x, _) => List(x)

      case Add(x, y) => x :: fv_id_or_imm(y)
      case Sub(x, y) => x :: fv_id_or_imm(y)
      case Ld(x, y, _) => x :: fv_id_or_imm(y)
      case LdDF(x, y, _) => x :: fv_id_or_imm(y)

      case St(x, y, z, _) => x :: y :: fv_id_or_imm(z)
      case StDF(x, y, z, _) => x :: y :: fv_id_or_imm(z)

      case FAddD(x, y) => List(x, y)
      case FSubD(x, y) => List(x, y)
      case FMulD(x, y) => List(x, y)
      case FDivD(x, y) => List(x, y)

      case IfEq(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(List(), fv(e1):::fv(e2))
      case IfLE(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(List(), fv(e1):::fv(e2))
      case IfGE(x, y, e1, e2) => x :: fv_id_or_imm(y) ::: remove_and_uniq(List(), fv(e1):::fv(e2))

      case IfFEq(x, y, e1, e2) => x :: y :: remove_and_uniq(List(), fv(e1):::fv(e2))
      case IfFLE(x, y, e1, e2) => x :: y :: remove_and_uniq(List(), fv(e1):::fv(e2))

      case CallCls(x, ys, zs) => x :: ys ::: zs
      case CallDir(_, ys, zs) => ys ::: zs
    }

  def fv(cont:List[Id.T], e:T):List[Id.T] =
    remove_and_uniq(List(), fv(e))
    /*
    e match {
      case Ans(exp) => fv_exp(exp)
      case Let((x,t), exp, e1) =>
        fv_exp(exp) ++ remove_and_uniq(List(x), fv(e1))
    }
    */

  def fv(e:T):List[Id.T] =
    e match {
      case Ans(exp) => fv_exp(exp)
      case Let((x,t), exp, e1) =>
        fv_exp(exp) ++ remove_and_uniq(List(x), fv(e1))
    }
    //remove_and_uniq(List(), fv(e))

  def concat(e1:T, xt:(Id.T, Type.T), e2:T):T =
    e1 match {
      case Ans(exp) => Let(xt, exp, e2)
      case Let(yt, exp, e1p) => Let(yt, exp, concat(e1p, xt, e2))
    }

  def align(i:Int):Int =
    if (i % 8 == 0) i else i+4
}
