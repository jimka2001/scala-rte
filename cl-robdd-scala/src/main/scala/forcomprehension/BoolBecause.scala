package forcomprehension

sealed abstract class BoolBecause {
  val toBoolean: Boolean = {
    this match {
      case True(_) => true
      case False(_) => false
    }
  }

  def ||(that: => BoolBecause):BoolBecause
  def &&(that: => BoolBecause):BoolBecause

  def unary_! : BoolBecause = {
    this match {
      case True(str) => False(str)
      case False(str) => True(str)
    }
  }

  def flatMap(f: String => BoolBecause): BoolBecause
  def map(f: String => String): BoolBecause
  def ++(str: String): BoolBecause
  def foreach[T](f: String => T): Unit
}

case class True(because: String) extends BoolBecause {
  def ||(that: => BoolBecause): BoolBecause = this

  def &&(that: => BoolBecause): BoolBecause = that

  def flatMap(f: String => BoolBecause): BoolBecause = {
    this
  }

  def map(f: String => String): BoolBecause = {
    this
  }

  def foreach[T](f: String => T): Unit = {
    f(because)
  }

  def ++(str: String): BoolBecause = {
    True(str ++ " " ++ because)
  }
}

case class False(because: String) extends BoolBecause {
  def ||(that: => BoolBecause): BoolBecause = that

  def &&(that: => BoolBecause): BoolBecause = this

  def flatMap(f: String => BoolBecause): BoolBecause = {
    f(because)
  }

  def map(f: String => String): BoolBecause = {
    False(f(because))
  }

  def foreach[T](f: String => T): Unit = ()

  def ++(str: String): BoolBecause = {
    False(str ++ " " ++ because)
  }
}

object BoolBecause {

  def forallM[T](items: LazyList[T], p: T => BoolBecause): BoolBecause = {
    def loop(acc: BoolBecause, tail: LazyList[T]): BoolBecause = {
      if (tail.isEmpty)
        acc
      else acc match {
        case False(_) => acc ++ s"counter example ${tail.head}"
        case True(_) =>
          loop(p(tail.head), tail.tail)
      }
    }

    loop(True(""), items)
  }

  def existsM[T](items: LazyList[T], p: T => BoolBecause): BoolBecause = {
    !(forallM[T](items, x => !(p(x))))
  }



  def assertM(a: BoolBecause) = {
    a match {
      case True(_) => ()
      case False(str) => throw new java.lang.AssertionError(str)
    }
  }
}

