package forcomprehension


import HeavyBool.Reason

sealed abstract class HeavyBool(because:Reason) {
  val toBoolean: Boolean = {
    this match {
      case HeavyTrue(_) => true
      case HeavyFalse(_) => false
    }
  }

  def ||(that: => HeavyBool):HeavyBool = {
    this match {
      case HeavyTrue(_) => this
      case HeavyFalse(_) => that
    }
  }

  def &&(that: => HeavyBool):HeavyBool = {
    this match {
      case HeavyTrue(_) => that
      case HeavyFalse(_) => this
    }
  }

  def unary_! : HeavyBool = {
    this match {
      case HeavyTrue(str) => HeavyFalse(str)
      case HeavyFalse(str) => HeavyTrue(str)
    }
  }

  def ==>(that: => HeavyBool): HeavyBool = {
    !this || that
  }

  def ++(any: Map[String,Any]): HeavyBool = {
    this match {
      case HeavyTrue(because) => HeavyTrue(any :: because)
      case HeavyFalse(because) => HeavyFalse(any :: because)
    }
  }

  def +| (reason:String): HeavyBool = this ++ Map("reason" -> reason)

  def conjTrue(another: Map[String,Any]): HeavyBool = {
    this match {
      case HeavyTrue(_) => this ++ another
      case HeavyFalse(_) => this
    }
  }

  def conjFalse(another: Map[String,Any]): HeavyBool = {
    this match {
      case HeavyTrue(_) => this
      case HeavyFalse(_) => this ++ another
    }
  }
}


case class HeavyTrue(because: Reason) extends HeavyBool(because) {}

case class HeavyFalse(because: Reason) extends HeavyBool(because) {}

object HeavyBool {
  type Reason = List[Map[String, Any]]
  val HTrue = HeavyTrue(List())
  val HFalse = HeavyFalse(List())

  def toHeavyBool(x:Boolean):HeavyBool = {
    if (x)
      HTrue
    else
      HFalse
  }

  def apply(test:Boolean, because:Reason):HeavyBool = {
    if (test)
      HeavyTrue(because)
    else
      HeavyFalse(because)
  }

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
        if(p(data.head).toBoolean)
          loop(data.tail)
        else
          HFalse ++ Map("witness" -> data.head)
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

