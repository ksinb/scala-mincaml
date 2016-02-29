/**
  * Created by Help Desk on 2016/02/08.
  */
/*
class Type
case class TypeUnit() extends Type
case class TypeBool(value:Boolean) extends Type
case class TypeInt(value:Int) extends Type
case class TypeFloat(value:Float) extends Type
case class TypeFun(value:Float) extends Type
case class TypeTuple(value:List[Type]) extends Type
case class TypeArray(value:Type) extends Type
case class TypeVar(value:Type, option:Option[Any], ref:AnyRef) extends Type

*/
package  scala_mincaml
object Type {
  sealed abstract class T()
  case class Unit() extends T
  case class Bool() extends T
  case class Int() extends T
  case class Float() extends T
  case class Fun(a:List[T], b:T) extends T // arguments are uncurried
  case class Tuple(a:List[T]) extends T
  case class Array(a:T) extends T
  case class Var(var a:Option[T]) extends T

  def gentyp():T = {
    new Var(None) // 新しい型変数を作る
  }
}



