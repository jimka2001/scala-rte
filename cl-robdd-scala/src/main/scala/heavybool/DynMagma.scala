package heavybool

import HeavyBool._
import cats.Foldable

case class DynMagma[T](gen1: () => LazyList[T],
                       op1: (T, T) => T,
                       member1: T => Boolean) extends Magma[T, LazyList] {
  override def toString: String = "dyn"

  def gen()(implicit ev: Foldable[LazyList]): LazyList[T] = gen1()

  def op(a: T, b: T): T = op1(a, b)

  def member(a: T): HeavyBool = member1(a) match {
    case true => HTrue ++ Map("reason" -> "a is a member",
                              "a" -> a)

    case false => HFalse ++ Map("reason" -> "a is not a member",
                                "a" -> a)
  }
}

