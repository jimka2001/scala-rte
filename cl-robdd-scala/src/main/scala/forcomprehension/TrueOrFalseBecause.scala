package forcomprehension

sealed abstract class TrueOrFalseBecause {
  val toBoolean: Boolean = {
    this match {
      case True(_) => true
      case False(_) => false
    }
  }

  def ||(that: => TrueOrFalseBecause):TrueOrFalseBecause
  def &&(that: => TrueOrFalseBecause):TrueOrFalseBecause

  def unary_! : TrueOrFalseBecause = {
    this match {
      case True(str) => False(str)
      case False(str) => True(str)
    }
  }

  def flatMap(f: String => TrueOrFalseBecause): TrueOrFalseBecause
  def map(f: String => String): TrueOrFalseBecause
  def ++(str: String): TrueOrFalseBecause
  def foreach[T](f: String => T): Unit
}

case class True(because: String) extends TrueOrFalseBecause {
  def ||(that: => TrueOrFalseBecause): TrueOrFalseBecause = this

  def &&(that: => TrueOrFalseBecause): TrueOrFalseBecause = that

  def flatMap(f: String => TrueOrFalseBecause): TrueOrFalseBecause = {
    this
  }

  def map(f: String => String): TrueOrFalseBecause = {
    this
  }

  def foreach[T](f: String => T): Unit = {
    f(because)
  }

  def ++(str: String): TrueOrFalseBecause = {
    True(str ++ " " ++ because)
  }
}

case class False(because: String) extends TrueOrFalseBecause {
  def ||(that: => TrueOrFalseBecause): TrueOrFalseBecause = that

  def &&(that: => TrueOrFalseBecause): TrueOrFalseBecause = this

  def flatMap(f: String => TrueOrFalseBecause): TrueOrFalseBecause = {
    f(because)
  }

  def map(f: String => String): TrueOrFalseBecause = {
    False(f(because))
  }

  def foreach[T](f: String => T): Unit = ()

  def ++(str: String): TrueOrFalseBecause = {
    False(str ++ " " ++ because)
  }
}

object TrueOrFalseBecause {

  def forallM[T](items: LazyList[T], p: T => TrueOrFalseBecause): TrueOrFalseBecause = {
    def loop(acc: TrueOrFalseBecause, tail: LazyList[T]): TrueOrFalseBecause = {
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

  def existsM[T](items: LazyList[T], p: T => TrueOrFalseBecause): TrueOrFalseBecause = {
    !(forallM[T](items, x => !(p(x))))
  }



  def assertM(a: TrueOrFalseBecause) = {
    a match {
      case True(_) => ()
      case False(str) => throw new java.lang.AssertionError(str)
    }
  }
}

