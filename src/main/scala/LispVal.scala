/**
  * Created by Help Desk on 2016/02/10.
  */
sealed trait LispVal
case class Num() extends LispVal
