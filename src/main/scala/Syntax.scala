/**
  * Created by Help Desk on 2016/02/05.
  */
package  scala_mincaml

sealed class T()
case class MUnit() extends T
case class MBool(t:Boolean) extends T
case class MInt(t:Int) extends T
case class MFloat(t:Float) extends T

case class MNot(t:T) extends T

case class MNeg(t:T) extends T
case class MFNeg(t: T) extends T

case class MAdd(t: Any, u: Any) extends T
case class MFAdd(t: T, u: T) extends T

case class MSub(t: T, u: T) extends T
case class MFSub(t: T, u: T) extends T

case class MFMul(t: T, u: T) extends T
case class MFDiv(t: T, u: T) extends T


case class MEq(t: T, u: T) extends T
case class MLE(t: T, u: T) extends T
case class MIf(t: T, u: T, v: T) extends T
case class MLet(a: T, b: T, c:T) extends T
case class MVar(t: String) extends T
case class MLetRec(a:MFundef, b:T) extends T

case class MApp(t: T, l:List[T]) extends T
case class MTuple(t: List[T]) extends T

case class MLetTuple(a:List[T], b:T, c:T) extends T

case class MArray(left: T, right: T) extends T
case class MGet(left: T, right: T) extends T
case class MPut(left: T, center: T, right: T) extends T

case class MFundef(l:List[T], t:T) extends T



