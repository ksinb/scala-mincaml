/**
  * Created by Help Desk on 2016/02/05.
  */

class Syntax
case class MUnit() extends Syntax
case class MBool(t:Boolean) extends Syntax
case class MInt(t:Int) extends Syntax
case class MFloat(t:Float) extends Syntax
case class MNot(t:Syntax) extends Syntax
case class MNeg(t:Syntax) extends Syntax
case class MAdd(t: Syntax, u: Syntax) extends Syntax
case class MSub(t: Syntax, u: Syntax) extends Syntax
case class MFNeg(t: Syntax) extends Syntax
case class MFAdd(t: Syntax, u: Syntax) extends Syntax
case class MFSub(t: Syntax, u: Syntax) extends Syntax
case class MFMul(t: Syntax, u: Syntax) extends Syntax
case class MFDiv(t: Syntax, u: Syntax) extends Syntax
case class MEq(t: Syntax, u: Syntax) extends Syntax
case class MLE(t: Syntax, u: Syntax) extends Syntax
case class MIf(t: Syntax, u: Syntax, v: Syntax) extends Syntax
case class MLet(t: Syntax, u: Syntax) extends Syntax
case class MVar(t: Id) extends Syntax

case class MLetRec(t:String) extends Syntax

case class MApp(t: List[List[Syntax]]) extends Syntax
case class MTuple(t: List[Syntax]) extends Syntax

case class MLetTuple(value:String) extends Syntax

case class MArray(left: Syntax, right: Syntax) extends Syntax
case class MGet(left: Syntax, right: Syntax) extends Syntax
case class MPut(left: Syntax, center: Syntax, right: Syntax) extends Syntax

case class MFundef(value:String) extends Syntax



