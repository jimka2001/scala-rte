package forcomprehension

package object forcomprehension {
  type Reason = List[Map[String, Any]]
}

import forcomprehension._

sealed abstract class HeavyBool(because:Reason) {
  val toBoolean: Boolean = {
    this match {
      case HeavyTrue(_) => true
      case HeavyFalse(_) => false
    }
  }

  def ||(that: => HeavyBool):HeavyBool
  def &&(that: => HeavyBool):HeavyBool

  def unary_! : HeavyBool = {
    this match {
      case HeavyTrue(str) => HeavyFalse(str)
      case HeavyFalse(str) => HeavyTrue(str)
    }
  }

  def ++(any: Map[String,Any]): HeavyBool
  def +| (reason:String): HeavyBool = {
    this ++ Map("reason" -> reason)
  }

  def conjTrue(another: Map[String,Any]): HeavyBool = this
  def conjFalse(another: Map[String,Any]): HeavyBool = this
}


case class HeavyTrue(because: forcomprehension.Reason) extends HeavyBool(because) {
  def ||(that: => HeavyBool): HeavyBool = this

  def &&(that: => HeavyBool): HeavyBool = that

  def ++(also:Map[String,Any]): HeavyBool = {
    HeavyTrue(also :: because)
  }
  override def conjTrue(another: Map[String,Any]): HeavyBool = this ++ another
}

case class HeavyFalse(because: forcomprehension.Reason) extends HeavyBool(because) {
  def ||(that: => HeavyBool): HeavyBool = that

  def &&(that: => HeavyBool): HeavyBool = this

  def ++(reason: Map[String,Any]): HeavyBool = {
    HeavyFalse(reason :: because)
  }
  override def conjFalse(another: Map[String,Any]): HeavyBool = this ++ another

}

object HeavyBool {

  def HTrue = HeavyTrue(List())
  def HFalse = HeavyFalse(List())

  def heavyIf(cond:HeavyBool,
              consequent: => HeavyBool,
              alternative: => HeavyBool) = {
    if (cond.toBoolean)
      consequent
    else
      alternative
  }

  def forallM[T](items: LazyList[T], p: T => HeavyBool): HeavyBool = {
    def loop(data: LazyList[T]): HeavyBool = {
      if (data.isEmpty)
        HTrue
      else {
        val next: HeavyBool = p(data.head)
        heavyIf(next,
                loop(data.tail),
                HTrue ++ Map("witness" -> data.head))

      }
    }
    loop(items)
  }

  def existsM[T](items: LazyList[T], p: T => HeavyBool): HeavyBool = {
    !(forallM[T](items, x => !(p(x))))
  }

  def assertM(a: HeavyBool) = {
    a match {
      case HeavyTrue(_) => ()
      case HeavyFalse(str) => throw new java.lang.AssertionError(str)
    }
  }
  def main(argv:Array[String]):Unit = {}
}

